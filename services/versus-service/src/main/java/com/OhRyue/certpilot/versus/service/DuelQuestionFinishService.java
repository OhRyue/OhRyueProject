package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * DUEL 모드 질문 종료 후처리 서비스 (단일 진입점)
 * 
 * SUBMIT과 TIMEOUT 경로가 동시에 호출되어도 Redis 락으로 1회만 실행되도록 보장
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DuelQuestionFinishService {

    private static final String EVENT_QUESTION_STARTED = "QUESTION_STARTED";
    private static final String EVENT_QUESTION_TIMEOUT_HANDLED = "QUESTION_TIMEOUT_HANDLED";
    private static final String EVENT_QUESTION_FINISHED = "QUESTION_FINISHED";
    private static final int QUESTION_INTERMISSION_SEC = 5;
    private static final long LOCK_TTL_MS = 8000; // 8초

    private final MatchRoomRepository roomRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final RedisLockService redisLockService;
    private final ScoreboardService scoreboardService;
    private final DuelMatchFinishService duelMatchFinishService;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;

    /**
     * 질문 종료 후처리 (단일 진입점)
     * 
     * @param roomId 방 ID
     * @param questionId 문제 ID
     * @param reason 종료 사유 (SUBMIT 또는 TIMEOUT)
     * @param triggeredByUserId 트리거한 사용자 ID (TIMEOUT의 경우 null)
     * @return FinishResult (다음 문제 정보 포함)
     */
    @Transactional
    public FinishResult finishQuestion(Long roomId, Long questionId, FinishReason reason, String triggeredByUserId) {
        Instant now = Instant.now();
        String lockKey = String.format("versus:lock:duel:question-finish:%d:%d", roomId, questionId);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("QUESTION_FINISH_LOCK_SKIPPED roomId={} q={} reason={}", roomId, questionId, reason);
            return FinishResult.skipped();
        }

        try {
            log.info("QUESTION_FINISH_ENTER roomId={} q={} reason={} triggeredBy={} now={}", 
                    roomId, questionId, reason, triggeredByUserId, now);

            // 2. 멱등성 방어: 이미 종료된 질문인지 확인
            boolean alreadyFinished = isQuestionAlreadyFinished(roomId, questionId);
            log.info("QUESTION_FINISH_CHECK roomId={} q={} alreadyFinished={}", 
                    roomId, questionId, alreadyFinished);
            
            if (alreadyFinished) {
                log.info("QUESTION_FINISH_ALREADY_CLOSED roomId={} q={} reason={} (이미 종료된 문제, 무시)", 
                        roomId, questionId, reason);
                return FinishResult.alreadyFinished();
            }

            // 3. 방 및 문제 조회
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));
            
            if (room.getMode() != MatchMode.DUEL) {
                log.warn("DuelQuestionFinishService called for non-DUEL room: roomId={}, mode={}", 
                        roomId, room.getMode());
                return FinishResult.skipped();
            }

            MatchQuestion question = questionRepository.findByRoomIdAndQuestionId(roomId, questionId)
                    .orElseThrow(() -> new IllegalStateException("Question not found: " + questionId));

            // 4. 미제출 유저 자동 오답 처리 (TIMEOUT 경로에서만 필요하지만, 안전을 위해 항상 확인)
            if (reason == FinishReason.TIMEOUT) {
                processUnansweredUsers(roomId, question);
            }

            // 5. 스코어보드 계산
            VersusDtos.ScoreBoardResp scoreboard = scoreboardService.computeScoreboard(room);

            // 6. 질문 종료 이벤트 기록
            recordQuestionFinished(roomId, questionId, reason, triggeredByUserId);

            // 7. 다음 문제로 이동 처리
            Optional<MatchQuestion> nextQuestion = findNextQuestion(roomId, question);
            boolean allQuestionsAnswered = checkAllQuestionsAnswered(roomId, participantRepository.countByRoomId(roomId));
            
            // nextQuestion=null이면 더 이상 진행할 문제가 없으므로 matchCompleted=true
            boolean matchCompleted = allQuestionsAnswered || nextQuestion.isEmpty();
            
            log.info("QUESTION_FINISH_NEXT_CALC roomId={} q={} nextQuestionId={} nextQuestion=null={} allQuestionsAnswered={} matchCompleted={}", 
                    roomId, questionId, 
                    nextQuestion.map(MatchQuestion::getQuestionId).orElse(null),
                    nextQuestion.isEmpty(),
                    allQuestionsAnswered,
                    matchCompleted);

            if (matchCompleted) {
                // 매치 종료 처리: DuelMatchFinishService로 위임
                try {
                    DuelMatchFinishService.MatchFinishResult matchResult = duelMatchFinishService.finishMatch(
                            roomId,
                            DuelMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE
                    );

                    if (matchResult.isProcessed()) {
                        log.info("QUESTION_FINISH_DONE roomId={} q={} matchCompleted=true matchFinishProcessed=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    } else if (matchResult.isAlreadyFinished()) {
                        log.info("QUESTION_FINISH_DONE roomId={} q={} matchCompleted=true matchAlreadyFinished=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    } else {
                        log.warn("QUESTION_FINISH_DONE roomId={} q={} matchCompleted=true matchFinishSkipped=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    }
                } catch (Exception e) {
                    log.error("Failed to finish match via DuelMatchFinishService: roomId={}, questionId={}, error={}",
                            roomId, questionId, e.getMessage(), e);
                    // 에러 발생 시 기존 로직으로 폴백 (안전장치)
                    String winner = scoreboard.items().isEmpty() ? null : scoreboard.items().get(0).userId();
                    recordEvent(roomId, "MATCH_FINISHED", Map.of(
                            "mode", "DUEL",
                            "winner", winner != null ? winner : "N/A"
                    ));
                    room.setStatus(MatchStatus.DONE);
                    roomRepository.save(room);
                    // 폴백 경로: DuelMatchFinishService를 통해 보상 지급 처리
                    // (DuelMatchFinishService가 이미 보상 지급 로직을 포함하고 있음)
                    try {
                        duelMatchFinishService.finishMatch(roomId, DuelMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE);
                    } catch (Exception finishException) {
                        log.error("Failed to finish match in fallback path: roomId={}, error={}", 
                                roomId, finishException.getMessage());
                    }
                    return FinishResult.completed(null, true);
                }
            } else if (nextQuestion.isPresent()) {
                // 다음 문제로 이동
                MatchQuestion next = nextQuestion.get();
                startNextQuestion(roomId, next);
                log.info("QUESTION_FINISH_DONE roomId={} q={} nextQuestion={} matchCompleted=false (다음 문제 시작)", 
                        roomId, questionId, next.getQuestionId());
                return FinishResult.completed(next.getQuestionId(), false);
            } else {
                // nextQuestion=null인데 matchCompleted=false로 남는 버그 수정
                // nextQuestion=null이면 더 이상 진행할 문제가 없으므로 매치 종료 처리
                log.warn("QUESTION_FINISH_NEXT_NULL roomId={} q={} nextQuestion=null -> matchCompleted=true로 강제 종료 처리", 
                        roomId, questionId);
                
                try {
                    DuelMatchFinishService.MatchFinishResult matchResult = duelMatchFinishService.finishMatch(
                            roomId,
                            DuelMatchFinishService.FinishMatchReason.LAST_QUESTION_DONE
                    );

                    if (matchResult.isProcessed()) {
                        log.info("QUESTION_FINISH_DONE roomId={} q={} nextQuestion=null matchCompleted=true matchFinishProcessed=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    } else if (matchResult.isAlreadyFinished()) {
                        log.info("QUESTION_FINISH_DONE roomId={} q={} nextQuestion=null matchCompleted=true matchAlreadyFinished=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    } else {
                        log.warn("QUESTION_FINISH_DONE roomId={} q={} nextQuestion=null matchCompleted=true matchFinishSkipped=true", 
                                roomId, questionId);
                        return FinishResult.completed(null, true);
                    }
                } catch (Exception e) {
                    log.error("QUESTION_FINISH_ERROR roomId={} q={} nextQuestion=null matchFinishFailed error={}", 
                            roomId, questionId, e.getMessage(), e);
                    // 에러 발생 시에도 matchCompleted=true로 반환
                    return FinishResult.completed(null, true);
                }
            }

        } catch (Exception e) {
            log.error("QUESTION_FINISH_ERROR roomId={} q={} reason={} ex={}", 
                    roomId, questionId, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 8. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 멱등성 방어: 이미 종료된 질문인지 확인
     * 
     * QUESTION_FINISHED 이벤트를 조회하여 payload에서 questionId를 정확히 비교
     */
    private boolean isQuestionAlreadyFinished(Long roomId, Long questionId) {
        try {
            // QUESTION_FINISHED 이벤트 조회
            List<MatchEvent> finishEvents = eventRepository.findByRoomIdAndEventType(roomId, EVENT_QUESTION_FINISHED);
            
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
                            log.debug("QUESTION_FINISHED 이벤트 발견: roomId={} q={} eventId={} createdAt={}", 
                                    roomId, questionId, event.getId(), event.getCreatedAt());
                            return true;
                        }
                    }
                } catch (Exception e) {
                    log.debug("Failed to parse payload for event {}: {}", event.getId(), e.getMessage());
                }
            }
            
            return false;
        } catch (Exception e) {
            log.warn("Failed to check if question already finished: roomId={} q={} error={}", 
                    roomId, questionId, e.getMessage());
            // 에러 발생 시 안전하게 false 반환 (중복 처리 방지보다는 정상 처리 우선)
            return false;
        }
    }

    /**
     * 미제출 유저 자동 오답 처리
     */
    private void processUnansweredUsers(Long roomId, MatchQuestion question) {
        Set<String> allParticipants = participantRepository.findByRoomId(roomId).stream()
                .map(MatchParticipant::getUserId)
                .collect(Collectors.toSet());

        Set<String> answeredUsers = answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId())
                .stream()
                .map(MatchAnswer::getUserId)
                .collect(Collectors.toSet());

        Set<String> unansweredUsers = allParticipants.stream()
                .filter(u -> !answeredUsers.contains(u))
                .collect(Collectors.toSet());

        for (String userId : unansweredUsers) {
            // 이미 답안이 있으면 skip
            if (answerRepository.findByRoomIdAndQuestionIdAndUserId(roomId, question.getQuestionId(), userId).isPresent()) {
                continue;
            }

            // 타임아웃 오답 저장
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

            // ANSWER_TIMEOUT 이벤트 기록
            recordEvent(roomId, "ANSWER_TIMEOUT", Map.of(
                    "userId", userId,
                    "questionId", question.getQuestionId(),
                    "round", question.getRoundNo(),
                    "phase", question.getPhase().name(),
                    "timeLimitSec", question.getTimeLimitSec()
            ));
        }
    }

    /**
     * 질문 종료 이벤트 기록
     */
    private void recordQuestionFinished(Long roomId, Long questionId, FinishReason reason, String triggeredByUserId) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("questionId", questionId);
        payload.put("reason", reason.name());
        payload.put("finishedAt", Instant.now().toString());
        if (triggeredByUserId != null) {
            payload.put("triggeredByUserId", triggeredByUserId);
        }

        recordEvent(roomId, EVENT_QUESTION_FINISHED, payload);

        // QUESTION_TIMEOUT_HANDLED 이벤트도 기록 (기존 로직과의 호환성)
        if (reason == FinishReason.TIMEOUT) {
            recordEvent(roomId, EVENT_QUESTION_TIMEOUT_HANDLED, Map.of(
                    "questionId", questionId,
                    "handledAt", Instant.now().toString()
            ));
        }
    }

    /**
     * 다음 문제 찾기
     */
    private Optional<MatchQuestion> findNextQuestion(Long roomId, MatchQuestion currentQuestion) {
        List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        
        log.info("DUEL_FIND_NEXT_QUESTION roomId={} currentQuestionId={} totalQuestions={} questionIds={}",
                roomId, currentQuestion.getQuestionId(), allQuestions.size(),
                allQuestions.stream().map(MatchQuestion::getQuestionId).toList());
        
        int currentIndex = -1;
        for (int i = 0; i < allQuestions.size(); i++) {
            if (allQuestions.get(i).getQuestionId().equals(currentQuestion.getQuestionId())) {
                currentIndex = i;
                break;
            }
        }
        
        if (currentIndex == -1) {
            log.warn("DUEL_FIND_NEXT_QUESTION_NOT_FOUND roomId={} currentQuestionId={} totalQuestions={}",
                    roomId, currentQuestion.getQuestionId(), allQuestions.size());
            return Optional.empty();
        }
        
        int nextIndex = currentIndex + 1;
        if (nextIndex < allQuestions.size()) {
            MatchQuestion nextQuestion = allQuestions.get(nextIndex);
            log.info("DUEL_FIND_NEXT_QUESTION_FOUND roomId={} currentIndex={} nextIndex={} totalQuestions={} nextQuestionId={}",
                    roomId, currentIndex, nextIndex, allQuestions.size(), nextQuestion.getQuestionId());
            return Optional.of(nextQuestion);
        } else {
            log.info("DUEL_FIND_NEXT_QUESTION_LAST roomId={} currentIndex={} totalQuestions={} (마지막 문제)",
                    roomId, currentIndex, allQuestions.size());
            return Optional.empty();
        }
    }

    /**
     * 다음 문제 시작
     */
    private void startNextQuestion(Long roomId, MatchQuestion nextQuestion) {
        // 쉬는 시간 시작 이벤트 기록
        Instant intermissionStart = Instant.now();
        recordEvent(roomId, "INTERMISSION_STARTED", Map.of(
                "nextQuestionId", nextQuestion.getQuestionId(),
                "nextRoundNo", nextQuestion.getRoundNo(),
                "nextPhase", nextQuestion.getPhase().name(),
                "durationSec", QUESTION_INTERMISSION_SEC,
                "startedAt", intermissionStart.toString(),
                "questionStartAt", intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC).toString()
        ));

        // 다음 문제 시작 시간 계산
        Instant questionStartTime = intermissionStart.plusSeconds(QUESTION_INTERMISSION_SEC);

        // 다음 문제 시작 이벤트 기록
        recordEvent(roomId, EVENT_QUESTION_STARTED, Map.of(
                "questionId", nextQuestion.getQuestionId(),
                "roundNo", nextQuestion.getRoundNo(),
                "phase", nextQuestion.getPhase().name(),
                "startedAt", questionStartTime.toString(),
                "allParticipants", true
        ));

        // ROUND_COMPLETED 이벤트 기록
        recordEvent(roomId, "ROUND_COMPLETED", Map.of(
                "mode", "DUEL",
                "round", nextQuestion.getRoundNo(),
                "phase", nextQuestion.getPhase().name()
        ));
    }

    /**
     * 이벤트 기록 및 실시간 브로드캐스트
     */
    private void recordEvent(Long roomId, String type, Map<String, Object> payload) {
        try {
            String payloadJson = payload == null || payload.isEmpty()
                    ? null
                    : objectMapper.writeValueAsString(payload);
            
            MatchEvent event = MatchEvent.builder()
                    .roomId(roomId)
                    .eventType(type)
                    .payloadJson(payloadJson)
                    .build();
            
            MatchEvent savedEvent = eventRepository.save(event);
            
            // 실시간 브로드캐스트
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            log.warn("Failed to serialize payload for event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
            // payload 없이 이벤트 저장 시도
            try {
                MatchEvent event = MatchEvent.builder()
                        .roomId(roomId)
                        .eventType(type)
                        .payloadJson(null)
                        .build();
                MatchEvent savedEvent = eventRepository.save(event);
                realtimeEventService.broadcastEvent(savedEvent);
            } catch (Exception ex) {
                log.warn("Failed to record event without payload: roomId={}, type={}, error={}", 
                        roomId, type, ex.getMessage());
            }
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }

    /**
     * 모든 문제 완료 확인
     */
    private boolean checkAllQuestionsAnswered(Long roomId, long participantCount) {
        List<MatchQuestion> allQuestions = questionRepository.findByRoomIdOrderByRoundNoAscOrderNoAsc(roomId);
        if (allQuestions.isEmpty()) {
            return false;
        }

        for (MatchQuestion q : allQuestions) {
            long answerCount = answerRepository.countByRoomIdAndQuestionId(roomId, q.getQuestionId());
            if (answerCount < participantCount) {
                return false;
            }
        }

        return true;
    }

    /**
     * 종료 사유
     */
    public enum FinishReason {
        SUBMIT,  // 답안 제출로 종료
        TIMEOUT  // 제한시간 만료로 종료
    }

    /**
     * 종료 결과
     */
    public static class FinishResult {
        private final boolean processed;
        private final boolean alreadyFinished;
        private final Long nextQuestionId;
        private final boolean matchCompleted;

        private FinishResult(boolean processed, boolean alreadyFinished, Long nextQuestionId, boolean matchCompleted) {
            this.processed = processed;
            this.alreadyFinished = alreadyFinished;
            this.nextQuestionId = nextQuestionId;
            this.matchCompleted = matchCompleted;
        }

        public static FinishResult skipped() {
            return new FinishResult(false, false, null, false);
        }

        public static FinishResult alreadyFinished() {
            return new FinishResult(false, true, null, false);
        }

        public static FinishResult completed(Long nextQuestionId, boolean matchCompleted) {
            return new FinishResult(true, false, nextQuestionId, matchCompleted);
        }

        public boolean isProcessed() {
            return processed;
        }

        public boolean isAlreadyFinished() {
            return alreadyFinished;
        }

        public Long getNextQuestionId() {
            return nextQuestionId;
        }

        public boolean isMatchCompleted() {
            return matchCompleted;
        }
    }
}

