package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchAnswer;
import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchQuestion;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.MatchAnswerRepository;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
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
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 시간 초과 시 자동 오답 처리 서비스
 *
 * 1) 한 문제(방+questionId)에서 timeout 처리/진행은 "딱 1번"만 수행
 *    - MatchEvent: QUESTION_TIMEOUT_HANDLED 기록으로 idempotent 보장
 * 2) 미제출 유저 timeout 오답은 "전원 저장" 후
 *    - handleModeAfterAnswer()는 마지막에 1번만 호출
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TimeoutAnswerService {

  private static final String EVENT_QUESTION_STARTED = "QUESTION_STARTED";
  private static final String EVENT_TIMEOUT_HANDLED = "QUESTION_TIMEOUT_HANDLED";
  private static final String EVENT_ANSWER_TIMEOUT = "ANSWER_TIMEOUT";

  private final MatchRoomRepository roomRepository;
  private final MatchQuestionRepository questionRepository;
  private final MatchAnswerRepository answerRepository;
  private final MatchParticipantRepository participantRepository;
  private final MatchEventRepository eventRepository;
  private final VersusService versusService;
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
   */
  @Transactional
  protected void processRoomTimeoutAnswers(Long roomId, Instant now) {
    MatchRoom room = roomRepository.findById(roomId).orElse(null);
    if (room == null || room.getStatus() != MatchStatus.ONGOING) {
      return;
    }

    // 현재 진행 중 문제(최근 QUESTION_STARTED 이벤트 기준)
    MatchQuestion currentQuestion = getCurrentQuestion(roomId);
    if (currentQuestion == null) {
      return;
    }

    // endTime 계산
    QuestionTimeInfo timeInfo = getQuestionTimeInfo(roomId, currentQuestion.getQuestionId());
    if (timeInfo == null || timeInfo.endTime == null) {
      return;
    }

    // 아직 시간 안 지났으면 skip
    if (now.isBefore(timeInfo.endTime)) {
      return;
    }

    // 이미 timeout 처리한 문제면 재처리 금지 (idempotent)
    if (alreadyHandledTimeout(roomId, currentQuestion.getQuestionId())) {
      return;
    }

    // 참가자 목록
    Set<String> allParticipants = participantRepository.findByRoomId(roomId).stream()
        .map(p -> p.getUserId())
        .collect(Collectors.toSet());

    // 해당 문제에 대해 이미 답한 유저들만 조회(방 전체 답을 전부 끌어오지 않도록 개선)
    Set<String> answeredUsers = answerRepository.findByRoomIdAndQuestionId(roomId, currentQuestion.getQuestionId())
        .stream()
        .map(MatchAnswer::getUserId)
        .collect(Collectors.toSet());

    Set<String> unansweredUsers = allParticipants.stream()
        .filter(u -> !answeredUsers.contains(u))
        .collect(Collectors.toSet());

    // 1) 미제출 유저 timeout 오답 저장(전원)
    if (!unansweredUsers.isEmpty()) {
      log.info("Processing timeout answers for room {}, question {} (orderNo: {}): {} users, endTime: {}, now: {}",
          roomId, currentQuestion.getQuestionId(), currentQuestion.getOrderNo(),
          unansweredUsers.size(), timeInfo.endTime, now);

      for (String userId : unansweredUsers) {
        saveTimeoutAnswer(roomId, currentQuestion, userId);
      }
    } else {
      log.info("Time limit expired for room {}, question {} (orderNo: {}): all users answered. Proceeding.",
          roomId, currentQuestion.getQuestionId(), currentQuestion.getOrderNo());
    }

    // 2) timeout 처리 완료 이벤트 기록(여기서 1번만)
    markTimeoutHandled(roomId, currentQuestion.getQuestionId(), now);

    // 3) 다음 진행/종료 판단은 딱 1번만 호출
    try {
      VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
      versusService.handleModeAfterAnswer(room, currentQuestion, scoreboard);
    } catch (Exception e) {
      log.error("Failed to process match progress after timeout handled: {}", e.getMessage(), e);
    }
  }

  private boolean alreadyHandledTimeout(Long roomId, Long questionId) {
    // 이미 기록된 이벤트가 있으면 "이번 문제 timeout 처리/진행"은 끝난 것으로 간주
    return eventRepository.existsByRoomIdAndEventTypeAndPayloadJsonContaining(
        roomId, EVENT_TIMEOUT_HANDLED, "\"questionId\":" + questionId
    );
  }

  private void markTimeoutHandled(Long roomId, Long questionId, Instant now) {
    try {
      String payloadJson = String.format("{\"questionId\":%d,\"handledAt\":\"%s\"}", questionId, now.toString());
      MatchEvent e = MatchEvent.builder()
          .roomId(roomId)
          .eventType(EVENT_TIMEOUT_HANDLED)
          .payloadJson(payloadJson)
          .build();
      eventRepository.save(e);
    } catch (Exception ex) {
      // idempotent 마커 저장 실패는 치명적일 수 있으므로 warn 이상으로 남김
      log.warn("Failed to save timeout handled marker: roomId={}, questionId={}, error={}",
          roomId, questionId, ex.getMessage());
    }
  }

  /**
   * timeout 오답 저장(단건)
   * - 중복 저장 방어(유니크 제약이 없을 수 있으므로 코드로 방어)
   */
  private void saveTimeoutAnswer(Long roomId, MatchQuestion question, String userId) {
    boolean alreadyAnswered = answerRepository
        .findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), userId)
        .isPresent();
    if (alreadyAnswered) return;

    MatchAnswer timeoutAnswer = MatchAnswer.builder()
        .roomId(roomId)
        .questionId(question.getQuestionId())
        .userId(userId)
        .roundNo(question.getRoundNo())
        .phase(question.getPhase())
        .correct(false)
        .timeMs(question.getTimeLimitSec() * 1000)
        .scoreDelta(0)
        .userAnswer("")
        .build();
    answerRepository.save(timeoutAnswer);

    MatchEvent timeoutEvent = MatchEvent.builder()
        .roomId(roomId)
        .eventType(EVENT_ANSWER_TIMEOUT)
        .payloadJson(String.format(
            "{\"userId\":\"%s\",\"questionId\":%d,\"round\":%d,\"phase\":\"%s\",\"timeLimitSec\":%d}",
            userId, question.getQuestionId(), question.getRoundNo(),
            question.getPhase().name(), question.getTimeLimitSec()))
        .build();
    eventRepository.save(timeoutEvent);

    log.debug("Auto-processed timeout answer: roomId={}, questionId={}, userId={}",
        roomId, question.getQuestionId(), userId);
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

  private static class QuestionTimeInfo {
    final Instant startTime;
    final Instant endTime;

    QuestionTimeInfo(Instant startTime, Instant endTime) {
      this.startTime = startTime;
      this.endTime = endTime;
    }
  }
}
