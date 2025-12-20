package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.fasterxml.jackson.core.type.TypeReference;
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
 * GOLDENBELL 모드 질문 종료 후처리 서비스 (단일 진입점)
 * 
 * 질문 종료 시 탈락 처리 및 전원 탈락 방지 로직을 담당합니다.
 * Redis 락으로 동시성 보장, 멱등성 체크로 재시도 방어.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GoldenBellQuestionFinishService {

    private static final String EVENT_QUESTION_FINISHED = "GB_QUESTION_FINISHED";
    private static final String EVENT_QUESTION_RETRY = "GB_QUESTION_RETRY_ON_FULL_ELIMINATION";
    private static final long LOCK_TTL_MS = 15000; // 15초

    private final MatchRoomRepository roomRepository;
    private final MatchQuestionRepository questionRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchEventRepository eventRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final RedisLockService redisLockService;
    private final VersusService versusService;
    private final RealtimeEventService realtimeEventService;
    private final GoldenBellRetryService retryService;
    private final ObjectMapper objectMapper;

    /**
     * 질문 종료 후처리 (단일 진입점)
     * 
     * @param roomId 방 ID
     * @param stepKey stepKey (예: "1:1:MAIN")
     * @param reason 종료 사유
     * @param triggeredByUserId 트리거한 사용자 ID
     * @return QuestionFinishResult
     */
    @Transactional
    public QuestionFinishResult finishQuestion(Long roomId, String stepKey, FinishReason reason, String triggeredByUserId) {
        String lockKey = String.format("versus:lock:GOLDENBELL:qfinish:%d:%s", roomId, stepKey);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("GB_QFINISH_LOCK_SKIPPED roomId={} stepKey={}", roomId, stepKey);
            return QuestionFinishResult.skipped();
        }

        try {
            log.info("GB_QFINISH_LOCK_ACQUIRED roomId={} stepKey={} reason={} triggeredBy={}", 
                    roomId, stepKey, reason, triggeredByUserId);

            // 2. 멱등성 방어: 이미 종료된 질문인지 확인
            if (isQuestionAlreadyFinished(roomId, stepKey)) {
                log.info("GB_QFINISH_ALREADY_CLOSED roomId={} stepKey={}", roomId, stepKey);
                return QuestionFinishResult.alreadyFinished();
            }

            // 3. 방 및 문제 조회
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

            if (room.getMode() != MatchMode.GOLDENBELL) {
                log.warn("GoldenBellQuestionFinishService called for non-GOLDENBELL room: roomId={}, mode={}", 
                        roomId, room.getMode());
                return QuestionFinishResult.skipped();
            }

            // stepKey 파싱
            String[] parts = stepKey.split(":");
            if (parts.length != 3) {
                throw new IllegalArgumentException("Invalid stepKey format: " + stepKey);
            }
            int roundNo = Integer.parseInt(parts[0]);
            int orderNo = Integer.parseInt(parts[1]);
            String phaseStr = parts[2];

            MatchQuestion question = questionRepository.findByRoomIdAndRoundNo(roomId, roundNo).stream()
                    .filter(q -> q.getOrderNo().equals(orderNo) && 
                            (q.getPhase() != null && q.getPhase().name().equals(phaseStr)))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Question not found: " + stepKey));

            // 4. 미제출 유저 자동 오답 처리
            if (reason == FinishReason.TIMEOUT) {
                processUnansweredUsers(roomId, question);
            }

            // 5. 채점 및 탈락 처리
            List<GoldenbellState> states = goldenbellStateRepository.findByRoomId(roomId);
            List<String> eliminatedUserIds = processElimination(roomId, question, states);

            // 6. 생존자 수 확인
            long aliveCount = states.stream()
                    .filter(GoldenbellState::isAlive)
                    .count();

            log.info("GB_QFINISH_ELIMINATION roomId={} stepKey={} aliveCount={} eliminated={}", 
                    roomId, stepKey, aliveCount, eliminatedUserIds.size());

            // 7. 전원 탈락 감지 및 재출제 처리
            if (aliveCount == 0) {
                log.warn("GB_FULL_ELIMINATION_DETECTED roomId={} stepKey={}", roomId, stepKey);
                return retryService.retryQuestionOnFullElimination(roomId, stepKey);
            }

            // 8. 질문 종료 이벤트 기록
            recordQuestionFinished(roomId, stepKey, reason, eliminatedUserIds, aliveCount);

            // 9. 다음 진행 판단
            VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
            String nextStepKey = decideNextStep(room, stepKey, aliveCount);

            if (nextStepKey == null) {
                // 매치 종료
                return QuestionFinishResult.completed(null, true);
            } else {
                // 다음 문제로 이동
                startNextQuestion(roomId, nextStepKey);
                return QuestionFinishResult.completed(nextStepKey, false);
            }

        } catch (Exception e) {
            log.error("GB_QFINISH_ERROR roomId={} stepKey={} reason={} ex={}", 
                    roomId, stepKey, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 10. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 미제출 유저 자동 오답 처리
     */
    private void processUnansweredUsers(Long roomId, MatchQuestion question) {
        List<GoldenbellState> aliveStates = goldenbellStateRepository.findByRoomId(roomId).stream()
                .filter(GoldenbellState::isAlive)
                .collect(Collectors.toList());

        List<String> aliveUserIds = aliveStates.stream()
                .map(GoldenbellState::getUserId)
                .collect(Collectors.toList());

        List<String> answeredUserIds = answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId())
                .stream()
                .map(com.OhRyue.certpilot.versus.domain.MatchAnswer::getUserId)
                .collect(Collectors.toList());

        // 미제출 유저 찾기
        List<String> unansweredUserIds = aliveUserIds.stream()
                .filter(userId -> !answeredUserIds.contains(userId))
                .collect(Collectors.toList());

        // 미제출 유저에게 자동 오답 저장
        for (String userId : unansweredUserIds) {
            com.OhRyue.certpilot.versus.domain.MatchAnswer autoAnswer = 
                    com.OhRyue.certpilot.versus.domain.MatchAnswer.builder()
                            .roomId(roomId)
                            .userId(userId)
                            .questionId(question.getQuestionId())
                            .userAnswer("")
                            .correct(false)
                            .timeMs(question.getTimeLimitSec() * 1000) // 타임아웃 시간
                            .scoreDelta(0)
                            .roundNo(question.getRoundNo())
                            .phase(question.getPhase())
                            .build();
            answerRepository.save(autoAnswer);
        }
    }

    /**
     * 탈락 처리 (오답/미제출 시 즉시 탈락)
     */
    private List<String> processElimination(Long roomId, MatchQuestion question, List<GoldenbellState> states) {
        List<String> eliminatedUserIds = new ArrayList<>();

        // 해당 문제의 답안 조회
        List<com.OhRyue.certpilot.versus.domain.MatchAnswer> answers = 
                answerRepository.findByRoomIdAndQuestionId(roomId, question.getQuestionId());

        Map<String, com.OhRyue.certpilot.versus.domain.MatchAnswer> answerMap = answers.stream()
                .collect(Collectors.toMap(
                        com.OhRyue.certpilot.versus.domain.MatchAnswer::getUserId,
                        answer -> answer
                ));

        // 살아있는 참가자 중 오답/미제출 처리
        for (GoldenbellState state : states) {
            if (!state.isAlive()) {
                continue; // 이미 탈락한 참가자는 스킵
            }

            com.OhRyue.certpilot.versus.domain.MatchAnswer answer = answerMap.get(state.getUserId());
            if (answer == null || !answer.isCorrect()) {
                // 미제출 또는 오답 → 즉시 탈락
                state.setAlive(false);
                eliminatedUserIds.add(state.getUserId());
            }
        }

        // 탈락 상태 저장
        if (!eliminatedUserIds.isEmpty()) {
            goldenbellStateRepository.saveAll(states);
        }

        return eliminatedUserIds;
    }

    /**
     * 다음 stepKey 결정
     */
    private String decideNextStep(MatchRoom room, String currentStepKey, long aliveCount) {
        String[] parts = currentStepKey.split(":");
        int currentRound = Integer.parseInt(parts[0]);
        int currentOrder = Integer.parseInt(parts[1]);
        String currentPhase = parts[2];

        // REVIVAL 페이즈는 1문제만
        if (MatchPhase.REVIVAL.name().equals(currentPhase)) {
            if (currentRound < 4) {
                return String.format("%d:1:MAIN", currentRound + 1);
            } else {
                return null; // 매치 종료
            }
        }

        // 라운드별 문제 수
        int questionsInRound = (currentRound == 3) ? 1 : 2;

        if (currentOrder < questionsInRound) {
            // 같은 라운드의 다음 문제
            return String.format("%d:%d:%s", currentRound, currentOrder + 1, currentPhase);
        } else {
            // 라운드의 마지막 문제 완료
            // 생존자 ≤5명이고 라운드 1~2이면 REVIVAL
            if (aliveCount <= 5 && currentRound <= 2) {
                return String.format("%d:1:REVIVAL", currentRound + 1);
            } else if (currentRound < 4) {
                // 다음 라운드로
                return String.format("%d:1:MAIN", currentRound + 1);
            } else {
                return null; // 매치 종료
            }
        }
    }

    /**
     * 다음 문제 시작
     */
    private void startNextQuestion(Long roomId, String nextStepKey) {
        // TODO: QUESTION_STARTED 이벤트 기록 및 브로드캐스트
        // stepKey 파싱하여 문제 조회 후 이벤트 발행
    }

    /**
     * 멱등성 방어: 이미 종료된 질문인지 확인
     */
    private boolean isQuestionAlreadyFinished(Long roomId, String stepKey) {
        List<MatchEvent> finishEvents = eventRepository.findByRoomIdAndEventType(roomId, EVENT_QUESTION_FINISHED);
        return finishEvents.stream()
                .anyMatch(event -> {
                    try {
                        if (event.getPayloadJson() == null) return false;
                        Map<String, Object> payload = objectMapper.readValue(
                                event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                        Object stepKeyObj = payload.get("stepKey");
                        return stepKeyObj != null && stepKey.equals(stepKeyObj.toString());
                    } catch (Exception e) {
                        return false;
                    }
                });
    }

    /**
     * 질문 종료 이벤트 기록
     */
    private void recordQuestionFinished(Long roomId, String stepKey, FinishReason reason, 
                                       List<String> eliminatedUserIds, long aliveCount) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("stepKey", stepKey);
        payload.put("reason", reason.name());
        payload.put("eliminated", eliminatedUserIds);
        payload.put("aliveCount", aliveCount);
        payload.put("finishedAt", Instant.now().toString());

        recordEvent(roomId, EVENT_QUESTION_FINISHED, payload);
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
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }

    /**
     * 종료 사유
     */
    public enum FinishReason {
        SUBMIT, TIMEOUT
    }

    /**
     * 종료 결과
     */
    public static class QuestionFinishResult {
        private final boolean processed;
        private final boolean skipped;
        private final boolean alreadyFinished;
        private final String nextStepKey;
        private final boolean matchCompleted;

        private QuestionFinishResult(boolean processed, boolean skipped, boolean alreadyFinished, 
                                    String nextStepKey, boolean matchCompleted) {
            this.processed = processed;
            this.skipped = skipped;
            this.alreadyFinished = alreadyFinished;
            this.nextStepKey = nextStepKey;
            this.matchCompleted = matchCompleted;
        }

        public static QuestionFinishResult skipped() {
            return new QuestionFinishResult(false, true, false, null, false);
        }

        public static QuestionFinishResult alreadyFinished() {
            return new QuestionFinishResult(false, false, true, null, false);
        }

        public static QuestionFinishResult completed(String nextStepKey, boolean matchCompleted) {
            return new QuestionFinishResult(true, false, false, nextStepKey, matchCompleted);
        }

        public boolean isProcessed() {
            return processed;
        }

        public boolean isSkipped() {
            return skipped;
        }

        public boolean isAlreadyFinished() {
            return alreadyFinished;
        }

        public String getNextStepKey() {
            return nextStepKey;
        }

        public boolean isMatchCompleted() {
            return matchCompleted;
        }
    }
}







