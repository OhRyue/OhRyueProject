package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.repository.MatchQuestionRepository;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 시간 초과 시 자동 오답 처리 서비스
 * 
 * 리팩토링 후: 타임아웃 감지만 수행하고, 질문 종료 후처리는 DuelQuestionFinishService로 위임
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TimeoutAnswerService {

  private static final String EVENT_QUESTION_STARTED = "QUESTION_STARTED";

  private final MatchRoomRepository roomRepository;
  private final MatchQuestionRepository questionRepository;
  private final MatchEventRepository eventRepository;
  private final DuelQuestionFinishService duelQuestionFinishService;
  private final RedisLockService redisLockService;
  private final ObjectMapper objectMapper;

  /**
   * 매 10초마다 실행: 시간 초과 답안 자동 처리
   *
   * 분산락을 사용하여 여러 인스턴스에서 중복 실행 방지
   */
  @Scheduled(fixedRate = 10000)
  public void processTimeoutAnswers() {
    Instant now = Instant.now();

    List<MatchRoom> ongoingRooms = roomRepository.findByStatus(MatchStatus.ONGOING);

    for (MatchRoom room : ongoingRooms) {
      try {
        // DUEL 모드만 처리
        if (room.getMode() != com.OhRyue.certpilot.versus.domain.MatchMode.DUEL) {
          continue;
        }

        // 방 단위 분산락(예: roomId 기반)
        // 락 타임아웃: 30초
        redisLockService.executeWithLock(room.getId(), 30, () -> {
          processRoomTimeoutAnswers(room.getId(), now);
          return null;
        });
      } catch (Exception e) {
        log.error("Failed to process timeout answers for room {}: {}",
            room.getId(), e.getMessage(), e);
      }
    }
  }

  /**
   * 특정 방의 시간 초과 답안 처리
   * - 분산락으로 보호됨
   * - 타임아웃 감지만 수행하고, 질문 종료 후처리는 DuelQuestionFinishService로 위임
   */
  @Transactional
  protected void processRoomTimeoutAnswers(Long roomId, Instant now) {
    MatchRoom room = roomRepository.findById(roomId).orElse(null);
    if (room == null) {
      return;
    }
    
    // 매치가 이미 종료되었으면 skip
    if (room.getStatus() != MatchStatus.ONGOING) {
      log.debug("TIMEOUT_SCHEDULER_SKIP roomId={} status={} (매치가 이미 종료됨)", roomId, room.getStatus());
      return;
    }

    // 현재 진행 중 문제(최근 QUESTION_STARTED 이벤트 기준)
    MatchQuestion currentQuestion = getCurrentQuestion(roomId);
    if (currentQuestion == null) {
      log.debug("TIMEOUT_SCHEDULER_SKIP roomId={} (현재 진행 중인 문제 없음)", roomId);
      return;
    }

    Long questionId = currentQuestion.getQuestionId();
    
    // 이미 종료된 문제인지 확인 (멱등성 보호)
    if (isQuestionAlreadyFinished(roomId, questionId)) {
      log.info("TIMEOUT_SCHEDULER_SKIP roomId={} q={} (이미 종료된 문제, 무시)", roomId, questionId);
      return;
    }

    // endTime 계산
    QuestionTimeInfo timeInfo = getQuestionTimeInfo(roomId, questionId);
    if (timeInfo == null || timeInfo.endTime == null) {
      log.debug("TIMEOUT_SCHEDULER_SKIP roomId={} q={} (시간 정보 없음)", roomId, questionId);
      return;
    }

    // 아직 시간 안 지났으면 skip
    if (now.isBefore(timeInfo.endTime)) {
      log.debug("TIMEOUT_SCHEDULER_SKIP roomId={} q={} now={} endTime={} (아직 시간 안 지남)", 
          roomId, questionId, now, timeInfo.endTime);
      return;
    }

    log.info("TIMEOUT_SCHEDULER_TRIGGER roomId={} q={} now={} endTime={} (타임아웃 감지)", 
        roomId, questionId, now, timeInfo.endTime);

    // 질문 종료 후처리는 DuelQuestionFinishService로 위임
    // (이미 종료된 질문인지 확인 및 락 처리는 DuelQuestionFinishService에서 수행)
    try {
      DuelQuestionFinishService.FinishResult result = duelQuestionFinishService.finishQuestion(
          roomId,
          questionId,
          DuelQuestionFinishService.FinishReason.TIMEOUT,
          null
      );
      
      if (result.isAlreadyFinished()) {
        log.info("TIMEOUT_SCHEDULER_RESULT roomId={} q={} alreadyFinished=true (이미 종료됨)", 
            roomId, questionId);
      } else if (result.isProcessed()) {
        log.info("TIMEOUT_SCHEDULER_RESULT roomId={} q={} processed=true matchCompleted={}", 
            roomId, questionId, result.isMatchCompleted());
      } else {
        log.debug("TIMEOUT_SCHEDULER_RESULT roomId={} q={} skipped (락 획득 실패)", 
            roomId, questionId);
      }
    } catch (Exception e) {
      log.error("TIMEOUT_SCHEDULER_ERROR roomId={} q={} error={}", 
          roomId, questionId, e.getMessage(), e);
    }
  }

  /**
   * 현재 진행 중인 문제 찾기 (가장 최근 QUESTION_STARTED 이벤트 사용)
   */
  private MatchQuestion getCurrentQuestion(Long roomId) {
    try {
      List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(roomId, EVENT_QUESTION_STARTED);

      Optional<MatchEvent> latestEvent = startEvents.stream()
          .max(Comparator.comparing(MatchEvent::getCreatedAt));

      if (latestEvent.isEmpty()) return null;

      MatchEvent event = latestEvent.get();
      if (event.getPayloadJson() == null) return null;

      Map<String, Object> payload = objectMapper.readValue(
          event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});

      Object questionIdObj = payload.get("questionId");
      if (questionIdObj == null) return null;

      Long questionId = Long.valueOf(questionIdObj.toString());
      return questionRepository.findByRoomIdAndQuestionId(roomId, questionId).orElse(null);

    } catch (Exception e) {
      log.debug("Failed to get current question: {}", e.getMessage());
      return null;
    }
  }

  /**
   * 문제 시작 시점 및 종료 시간 조회
   */
  private QuestionTimeInfo getQuestionTimeInfo(Long roomId, Long questionId) {
    try {
      List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(roomId, EVENT_QUESTION_STARTED);

      Optional<MatchEvent> questionEvent = startEvents.stream()
          .filter(e -> {
            if (e.getPayloadJson() == null) return false;
            try {
              Map<String, Object> payload = objectMapper.readValue(
                  e.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
              Object qId = payload.get("questionId");
              return qId != null && questionId.equals(Long.valueOf(qId.toString()));
            } catch (Exception ex) {
              return false;
            }
          })
          .max(Comparator.comparing(MatchEvent::getCreatedAt));

      if (questionEvent.isEmpty()) return null;

      MatchEvent event = questionEvent.get();
      if (event.getPayloadJson() == null) return null;

      Map<String, Object> payload = objectMapper.readValue(
          event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});

      String startedAtStr = (String) payload.get("startedAt");
      Instant startTime = startedAtStr != null ? Instant.parse(startedAtStr) : event.getCreatedAt();

      Optional<MatchQuestion> mq = questionRepository.findByRoomIdAndQuestionId(roomId, questionId);
      if (mq.isEmpty()) return null;

      Instant endTime = startTime.plusSeconds(mq.get().getTimeLimitSec());
      return new QuestionTimeInfo(startTime, endTime);

    } catch (Exception e) {
      log.debug("Failed to get question time info: {}", e.getMessage());
      return null;
    }
  }

  /**
   * 이미 종료된 문제인지 확인 (멱등성 보호)
   */
  private boolean isQuestionAlreadyFinished(Long roomId, Long questionId) {
    try {
      List<MatchEvent> finishEvents = eventRepository.findByRoomIdAndEventType(roomId, "QUESTION_FINISHED");
      
      for (MatchEvent event : finishEvents) {
        if (event.getPayloadJson() == null) {
          continue;
        }
        
        try {
          Map<String, Object> payload = objectMapper.readValue(
              event.getPayloadJson(), 
              new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {}
          );
          
          Object qIdObj = payload.get("questionId");
          if (qIdObj != null) {
            Long qId = qIdObj instanceof Number 
                ? ((Number) qIdObj).longValue() 
                : Long.valueOf(qIdObj.toString());
            
            if (qId.equals(questionId)) {
              return true;
            }
          }
        } catch (Exception e) {
          // payload 파싱 실패는 무시
        }
      }
      
      return false;
    } catch (Exception e) {
      log.debug("Failed to check if question already finished: roomId={} q={} error={}", 
          roomId, questionId, e.getMessage());
      return false;
    }
  }

  private static class QuestionTimeInfo {
    final Instant startTime;
    final Instant endTime;

    QuestionTimeInfo(Instant startTime, Instant endTime) {
      this.startTime = startTime;
      this.endTime = endTime;
    }
  }
}
