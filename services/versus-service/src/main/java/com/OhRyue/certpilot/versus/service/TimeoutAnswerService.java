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
 * - 시간 내 답하지 않은 사용자 자동 오답 처리
 * - 모든 답변이 오기까지 기다리지 않고 시간 제한 지나면 자동 진행
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TimeoutAnswerService {

    private final MatchRoomRepository roomRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final VersusService versusService;
    private final ObjectMapper objectMapper;

    /**
     * 매 10초마다 실행: 시간 초과 답안 자동 처리
     */
    @Scheduled(fixedRate = 10000) // 10초마다
    @Transactional
    public void processTimeoutAnswers() {
        Instant now = Instant.now();
        
        // 진행 중인 방 찾기
        List<MatchRoom> ongoingRooms = roomRepository.findByStatus(MatchStatus.ONGOING);
        
        for (MatchRoom room : ongoingRooms) {
            try {
                processRoomTimeoutAnswers(room, now);
            } catch (Exception e) {
                log.error("Failed to process timeout answers for room {}: {}", 
                    room.getId(), e.getMessage(), e);
            }
        }
    }

    /**
     * 특정 방의 시간 초과 답안 처리
     */
    private void processRoomTimeoutAnswers(MatchRoom room, Instant now) {
        // 현재 진행 중인 문제 찾기 (가장 최근 QUESTION_STARTED 이벤트 사용)
        MatchQuestion currentQuestion = getCurrentQuestion(room.getId());
        
        if (currentQuestion == null) {
            return; // 현재 진행 중인 문제가 없음
        }

        // 모든 참가자 목록
        Set<String> allParticipants = participantRepository.findByRoomId(room.getId())
            .stream()
            .map(p -> p.getUserId())
            .collect(Collectors.toSet());

        // 이미 답안을 제출한 사용자들
        Set<String> answeredUsers = answerRepository
            .findByRoomId(room.getId())
            .stream()
            .filter(a -> a.getQuestionId().equals(currentQuestion.getQuestionId()))
            .map(MatchAnswer::getUserId)
            .collect(Collectors.toSet());

        // 답안을 제출하지 않은 사용자들
        Set<String> unansweredUsers = allParticipants.stream()
            .filter(userId -> !answeredUsers.contains(userId))
            .collect(Collectors.toSet());

        if (unansweredUsers.isEmpty()) {
            return; // 모든 사용자가 답안 제출
        }

        // 문제 시작 시점 및 종료 시간 확인 (QUESTION_STARTED 이벤트)
        QuestionTimeInfo timeInfo = getQuestionTimeInfo(room.getId(), currentQuestion.getQuestionId());
        if (timeInfo == null || timeInfo.endTime == null) {
            return; // 문제 시작 이벤트가 없거나 종료 시간을 계산할 수 없음
        }

        // endTime이 지났는지 확인
        if (now.isAfter(timeInfo.endTime) || now.equals(timeInfo.endTime)) {
            // 시간 초과: 자동 오답 처리
            log.info("Processing timeout answers for room {}, question {} (orderNo: {}): {} users, endTime: {}, now: {}", 
                room.getId(), currentQuestion.getQuestionId(), currentQuestion.getOrderNo(), 
                unansweredUsers.size(), timeInfo.endTime, now);
            
            for (String userId : unansweredUsers) {
                processTimeoutAnswer(room, currentQuestion, userId, currentQuestion.getTimeLimitSec());
            }
        }
    }

    /**
     * 시간 초과 답안 처리 (자동 오답)
     */
    private void processTimeoutAnswer(MatchRoom room, MatchQuestion question, 
                                      String userId, int timeLimitSec) {
        // 이미 처리되었는지 확인
        boolean alreadyAnswered = answerRepository
            .findByRoomIdAndQuestionIdAndUserId(room.getId(), question.getQuestionId(), userId)
            .isPresent();
        
        if (alreadyAnswered) {
            return; // 이미 답안 제출됨
        }

        // 자동 오답 생성 (시간 제한 내 제출 못함)
        MatchAnswer timeoutAnswer = MatchAnswer.builder()
            .roomId(room.getId())
            .questionId(question.getQuestionId())
            .userId(userId)
            .roundNo(question.getRoundNo())
            .phase(question.getPhase())
            .correct(false) // 자동 오답
            .timeMs(timeLimitSec * 1000) // 시간 제한을 시간으로 설정
            .scoreDelta(0) // 오답이므로 점수 변화 없음
            .userAnswer("") // 공백으로 처리
            .build();

        answerRepository.save(timeoutAnswer);

        // 이벤트 기록
        MatchEvent timeoutEvent = MatchEvent.builder()
            .roomId(room.getId())
            .eventType("ANSWER_TIMEOUT")
            .payloadJson(String.format(
                "{\"userId\":\"%s\",\"questionId\":%d,\"round\":%d,\"phase\":\"%s\",\"timeLimitSec\":%d}",
                userId, question.getQuestionId(), question.getRoundNo(), 
                question.getPhase().name(), timeLimitSec))
            .build();
        eventRepository.save(timeoutEvent);

        log.debug("Auto-processed timeout answer for room {}, question {}, user {}", 
            room.getId(), question.getQuestionId(), userId);

        // 매치 진행 상태 확인 (모든 답변이 오지 않아도 시간 제한 지나면 진행)
        try {
            VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
            versusService.handleModeAfterAnswer(room, question, scoreboard);
        } catch (Exception e) {
            log.error("Failed to process match progress after timeout answer: {}", e.getMessage());
        }
    }

    /**
     * 현재 진행 중인 문제 찾기 (가장 최근 QUESTION_STARTED 이벤트 사용)
     */
    private MatchQuestion getCurrentQuestion(Long roomId) {
        try {
            // 가장 최근 QUESTION_STARTED 이벤트 찾기
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");
            
            // createdAt 기준으로 최신순 정렬
            Optional<MatchEvent> latestEvent = startEvents.stream()
                    .max(Comparator.comparing(MatchEvent::getCreatedAt));

            if (latestEvent.isPresent()) {
                try {
                    MatchEvent event = latestEvent.get();
                    if (event.getPayloadJson() == null) {
                        return null;
                    }
                    
                    Map<String, Object> payload = objectMapper.readValue(
                            event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                    
                    Object questionIdObj = payload.get("questionId");
                    if (questionIdObj == null) {
                        return null;
                    }
                    
                    Long questionId = Long.valueOf(questionIdObj.toString());
                    return questionRepository.findByRoomIdAndQuestionId(roomId, questionId)
                            .orElse(null);
                } catch (Exception e) {
                    log.debug("Failed to parse current question from event: {}", e.getMessage());
                    return null;
                }
            }
        } catch (Exception e) {
            log.debug("Failed to get current question: {}", e.getMessage());
        }
        
        return null;
    }

    /**
     * 문제 시작 시점 및 종료 시간 조회
     */
    private QuestionTimeInfo getQuestionTimeInfo(Long roomId, Long questionId) {
        try {
            List<MatchEvent> startEvents = eventRepository.findByRoomIdAndEventTypeContaining(
                    roomId, "QUESTION_STARTED");
            
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

            if (questionEvent.isPresent()) {
                try {
                    MatchEvent event = questionEvent.get();
                    if (event.getPayloadJson() == null) {
                        return null;
                    }
                    
                    Map<String, Object> payload = objectMapper.readValue(
                            event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                    
                    // startedAt 시간 가져오기
                    String startedAtStr = (String) payload.get("startedAt");
                    Instant startTime = startedAtStr != null 
                            ? Instant.parse(startedAtStr) 
                            : event.getCreatedAt();
                    
                    // MatchQuestion에서 timeLimitSec 가져오기
                    Optional<MatchQuestion> matchQuestion = questionRepository.findByRoomIdAndQuestionId(roomId, questionId);
                    if (matchQuestion.isPresent()) {
                        MatchQuestion q = matchQuestion.get();
                        // 종료 시간 계산: 시작 시간 + 시간 제한
                        Instant endTime = startTime.plusSeconds(q.getTimeLimitSec());
                        return new QuestionTimeInfo(startTime, endTime);
                    }
                } catch (Exception e) {
                    log.debug("Failed to parse question time info: {}", e.getMessage());
                    return null;
                }
            }
        } catch (Exception e) {
            log.debug("Failed to get question time info: {}", e.getMessage());
        }
        
        return null;
    }

    /**
     * 문제 시간 정보를 담는 내부 클래스
     */
    @SuppressWarnings("unused")
    private static class QuestionTimeInfo {
        @SuppressWarnings("unused")
        final Instant startTime;
        final Instant endTime;

        QuestionTimeInfo(Instant startTime, Instant endTime) {
            this.startTime = startTime;
            this.endTime = endTime;
        }
    }
}

