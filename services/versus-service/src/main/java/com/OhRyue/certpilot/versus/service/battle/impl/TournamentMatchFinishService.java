package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.client.ProgressServiceClient;
import com.OhRyue.certpilot.versus.config.MonitoringConfig;
import com.OhRyue.certpilot.versus.domain.*;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import com.OhRyue.certpilot.versus.repository.*;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.OhRyue.certpilot.versus.service.RewardRetryService;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.micrometer.core.instrument.Timer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * TOURNAMENT 모드 매치 종료 후처리 서비스 (단일 진입점)
 * 
 * 우승자 확정 및 progress-service 보상 지급을 담당합니다.
 * Redis 락으로 동시성 보장, 멱등성 플래그로 재시도 방어.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TournamentMatchFinishService {

    private static final String EVENT_MATCH_FINISHED = "MATCH_FINISHED";
    private static final String EVENT_XP_REWARDED = "XP_REWARDED";
    private static final long LOCK_TTL_MS = 30000; // 30초

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchEventRepository eventRepository;
    private final RedisLockService redisLockService;
    private final VersusService versusService;
    private final RealtimeEventService realtimeEventService;
    private final ProgressServiceClient progressServiceClient;
    private final MonitoringConfig monitoringConfig;
    private final RewardRetryService rewardRetryService;
    private final ObjectMapper objectMapper;

    /**
     * 매치 종료 후처리 (단일 진입점)
     * 
     * @param roomId 방 ID
     * @param reason 종료 사유
     * @return MatchFinishResult
     */
    @Transactional
    public MatchFinishResult finishMatch(Long roomId, FinishMatchReason reason) {
        String lockKey = String.format("versus:lock:TOURNAMENT:mfinish:%d", roomId);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("TMT_MFINISH_LOCK_SKIPPED roomId={} reason={}", roomId, reason);
            return MatchFinishResult.skipped();
        }

        try {
            log.info("TMT_MFINISH_LOCK_ACQUIRED roomId={} reason={}", roomId, reason);

            // 2. 멱등성 방어: 이미 종료된 매치인지 확인
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

            if (room.getMode() != MatchMode.TOURNAMENT) {
                log.warn("TournamentMatchFinishService called for non-TOURNAMENT room: roomId={}, mode={}", 
                        roomId, room.getMode());
                return MatchFinishResult.skipped();
            }

            if (isMatchAlreadyFinished(room)) {
                log.info("TMT_MFINISH_ALREADY_DONE roomId={}", roomId);
                return MatchFinishResult.alreadyFinished();
            }

            // 3. 스코어보드 계산
            VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
            if (scoreboard.items().isEmpty()) {
                log.warn("No participants in scoreboard for room {}, skipping match finish", roomId);
                return MatchFinishResult.skipped();
            }

            // 4. 우승자 결정 (tie-breaker 규칙 적용)
            List<MatchParticipant> participantEntities = participantRepository.findByRoomId(roomId);
            String winner = resolveWinner(room, scoreboard, participantEntities);

            // 5. 매치 종료 이벤트 기록
            recordMatchFinished(roomId, reason, winner);

            // 6. 방 상태 변경
            room.setStatus(MatchStatus.DONE);
            roomRepository.save(room);

            // 7. XP 지급 (우승자 1명만, 1회만)
            boolean xpGranted = grantXpReward(room, scoreboard, winner, participantEntities);

            log.info("TMT_MFINISH_DONE roomId={} winner={} xpGranted={}", roomId, winner, xpGranted);
            return MatchFinishResult.completed(winner, xpGranted);

        } catch (Exception e) {
            log.error("TMT_MFINISH_ERROR roomId={} reason={} ex={}", 
                    roomId, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 8. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 멱등성 방어: 이미 종료된 매치인지 확인
     */
    private boolean isMatchAlreadyFinished(MatchRoom room) {
        // 1. 방 상태 확인
        if (room.getStatus() == MatchStatus.DONE) {
            return true;
        }

        // 2. resultReported 플래그 확인
        if (room.getResultReported() != null && room.getResultReported()) {
            return true;
        }

        // 3. MATCH_FINISHED 이벤트 확인
        List<MatchEvent> finishEvents = eventRepository.findByRoomIdAndEventType(room.getId(), EVENT_MATCH_FINISHED);
        if (!finishEvents.isEmpty()) {
            boolean hasTournamentFinishEvent = finishEvents.stream()
                    .anyMatch(e -> {
                        try {
                            if (e.getPayloadJson() == null) return false;
                            Map<String, Object> payload = objectMapper.readValue(
                                    e.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                            Object mode = payload.get("mode");
                            return "TOURNAMENT".equals(mode);
                        } catch (Exception ex) {
                            return false;
                        }
                    });
            if (hasTournamentFinishEvent) {
                return true;
            }
        }

        return false;
    }

    /**
     * 우승자 결정 (tie-breaker 규칙 적용)
     */
    private String resolveWinner(MatchRoom room, VersusDtos.ScoreBoardResp scoreboard, 
                                List<MatchParticipant> participants) {
        if (scoreboard.items().isEmpty()) {
            return null;
        }

        // 1순위: 점수 (내림차순)
        // 2순위: 총 제출속도 합산 (오름차순, 빠른 사람이 우위)
        // 3순위: 마지막 라운드 제출속도 (오름차순)
        // 4순위: userId 오름차순 (deterministic tie-breaker)

        List<VersusDtos.ScoreBoardItem> sorted = scoreboard.items().stream()
                .sorted((a, b) -> {
                    // 1순위: 점수
                    int scoreCompare = Integer.compare(b.score(), a.score());
                    if (scoreCompare != 0) return scoreCompare;

                    // 2순위: 총 제출속도 합산
                    long timeA = Optional.ofNullable(a.totalTimeMs()).orElse(0L);
                    long timeB = Optional.ofNullable(b.totalTimeMs()).orElse(0L);
                    int timeCompare = Long.compare(timeA, timeB);
                    if (timeCompare != 0) return timeCompare;

                    // 3순위: 마지막 라운드 제출속도
                    // TODO: 마지막 라운드(3라운드) 제출속도 계산
                    // 현재는 총 제출속도로 대체

                    // 4순위: userId 오름차순
                    return a.userId().compareTo(b.userId());
                })
                .collect(Collectors.toList());

        return sorted.get(0).userId();
    }

    /**
     * 매치 종료 이벤트 기록
     */
    private void recordMatchFinished(Long roomId, FinishMatchReason reason, String winner) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("mode", "TOURNAMENT");
        payload.put("winner", winner != null ? winner : "NONE");
        payload.put("reason", reason.name());
        payload.put("finishedAt", Instant.now().toString());

        recordEvent(roomId, EVENT_MATCH_FINISHED, payload);
    }

    /**
     * XP 지급 (progress-service 호출, 우승자 1명만)
     */
    private boolean grantXpReward(MatchRoom room, VersusDtos.ScoreBoardResp scoreboard, 
                                  String winner, List<MatchParticipant> participantEntities) {
        // 이미 지급되었는지 확인 (이중 방어)
        if (room.getResultReported() != null && room.getResultReported()) {
            log.info("XP already granted for room {}, skipping", room.getId());
            return false;
        }

        Timer.Sample timer = monitoringConfig.startTimer("notifyProgressService");

        try {
            Long roomId = room.getId();
            String examMode = extractExamMode(room);

            // 우승자만 포함
            MatchParticipant winnerParticipant = participantEntities.stream()
                    .filter(p -> p.getUserId().equals(winner))
                    .findFirst()
                    .orElse(null);

            if (winnerParticipant == null) {
                log.warn("Winner participant not found: roomId={}, winner={}", roomId, winner);
                return false;
            }

            // 우승자 결과 수집
            List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswers(roomId, winner);
            VersusDtos.ScoreBoardItem winnerItem = scoreboard.items().stream()
                    .filter(item -> item.userId().equals(winner))
                    .findFirst()
                    .orElse(null);

            Integer score = Optional.ofNullable(winnerParticipant.getFinalScore())
                    .orElse(winnerItem != null ? winnerItem.score() : 0);
            Integer rank = 1; // 우승자는 항상 1위
            Integer correctCount = winnerItem != null ? winnerItem.correctCount() : 0;
            Integer totalCount = winnerItem != null ? winnerItem.totalCount() : 0;
            Long totalTimeMs = Optional.ofNullable(winnerItem)
                    .map(VersusDtos.ScoreBoardItem::totalTimeMs)
                    .orElse(0L);

            ProgressServiceClient.ParticipantResult winnerResult = new ProgressServiceClient.ParticipantResult(
                    winner,
                    score,
                    rank,
                    correctCount,
                    totalCount,
                    totalTimeMs,
                    answers
            );

            Integer questionCount = (int) answerRepository.countDistinctQuestionIdByRoomId(roomId);
            if (questionCount == 0) {
                questionCount = 9; // TOURNAMENT는 9문제 고정
            }
            Long durationMs = calculateMatchDuration(room);

            ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                    room.getMode().name(),
                    room.getId(),
                    winner,
                    List.of(winnerResult), // 우승자 1명만
                    questionCount,
                    durationMs,
                    examMode,
                    room.getIsBotMatch()
            );

            log.info("TMT_REWARD_CALL roomId={} winner={} mode=TOURNAMENT participants=1 questions={} duration={}ms",
                    room.getId(), winner, questionCount, durationMs);

            ProgressServiceClient.VersusResultResponse xpResponse = progressServiceClient.recordVersusResult(request);
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "success");
            log.info("Successfully notified progress-service for TOURNAMENT room {} completion", room.getId());

            // XP 결과 이벤트 저장 후 결과 보고 플래그 업데이트
            try {
                recordEvent(roomId, EVENT_XP_REWARDED, Map.of("xpResults", xpResponse.xpResults()));
                room.setResultReported(true);
                roomRepository.save(room);
                log.info("XP result stored as event for room {} and resultReported set", room.getId());
            } catch (Exception e) {
                log.warn("Failed to store XP_REWARDED event for room {}: {}", room.getId(), e.getMessage());
            }

            return true;

        } catch (Exception e) {
            monitoringConfig.recordTimer(timer, "notifyProgressService", "status", "failure");
            monitoringConfig.recordRewardFailure(room.getId().toString(), "all", e);
            log.error("Failed to notify progress-service for room {}: {}", room.getId(), e.getMessage(), e);

            // 비동기 재시도 큐에 추가
            try {
                String examMode = extractExamMode(room);
                VersusDtos.ScoreBoardItem winnerItem = scoreboard.items().stream()
                        .filter(item -> item.userId().equals(winner))
                        .findFirst()
                        .orElse(null);

                if (winnerItem != null) {
                    List<ProgressServiceClient.AnswerDetail> answers = collectParticipantAnswers(room.getId(), winner);
                    Long totalTimeMs = Optional.ofNullable(winnerItem.totalTimeMs()).orElse(0L);
                    ProgressServiceClient.ParticipantResult winnerResult = new ProgressServiceClient.ParticipantResult(
                            winner,
                            winnerItem.score(),
                            1,
                            winnerItem.correctCount(),
                            winnerItem.totalCount(),
                            totalTimeMs,
                            answers
                    );

                    Integer questionCount = 9; // TOURNAMENT는 9문제 고정
                    Long durationMs = calculateMatchDuration(room);

                    ProgressServiceClient.VersusResultRequest request = new ProgressServiceClient.VersusResultRequest(
                            room.getMode().name(),
                            room.getId(),
                            winner,
                            List.of(winnerResult),
                            questionCount,
                            durationMs,
                            examMode,
                            room.getIsBotMatch()
                    );

                    rewardRetryService.retryRewardPayment(room.getId(), request);
                    log.info("보상 지급 재시도 큐에 추가: roomId={}", room.getId());
                }
            } catch (Exception retryException) {
                log.error("보상 지급 재시도 큐 추가 실패: roomId={}", room.getId(), retryException);
            }

            return false;
        }
    }

    /**
     * 매치 지속 시간 계산
     */
    private Long calculateMatchDuration(MatchRoom room) {
        Optional<MatchEvent> startEvent = eventRepository.findByRoomIdAndEventType(room.getId(), "MATCH_STARTED")
                .stream()
                .findFirst();

        if (startEvent.isPresent()) {
            return Duration.between(startEvent.get().getCreatedAt(), Instant.now()).toMillis();
        }
        return 0L;
    }

    /**
     * 방의 scopeJson에서 examMode 추출
     */
    private String extractExamMode(MatchRoom room) {
        try {
            if (room.getScopeJson() == null || room.getScopeJson().isBlank()) {
                return null;
            }
            Map<String, Object> scope = objectMapper.readValue(
                    room.getScopeJson(), new TypeReference<Map<String, Object>>() {});
            Object examModeObj = scope.get("examMode");
            if (examModeObj != null) {
                return examModeObj.toString();
            }
        } catch (Exception e) {
            log.debug("Failed to extract examMode from room {}: {}", room.getId(), e.getMessage());
        }
        return null;
    }

    /**
     * 참가자의 개별 답안 정보 수집
     */
    private List<ProgressServiceClient.AnswerDetail> collectParticipantAnswers(Long roomId, String userId) {
        List<com.OhRyue.certpilot.versus.domain.MatchAnswer> answers = answerRepository.findByRoomId(roomId).stream()
                .filter(answer -> answer.getUserId().equals(userId))
                .toList();
        return answers.stream()
                .map(answer -> new ProgressServiceClient.AnswerDetail(
                        answer.getQuestionId(),
                        answer.getUserAnswer(),
                        answer.isCorrect(),
                        answer.getTimeMs(),
                        answer.getScoreDelta(),
                        answer.getRoundNo(),
                        answer.getPhase() != null ? answer.getPhase().name() : null
                ))
                .toList();
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
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            log.warn("Failed to serialize payload for event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
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
     * 종료 사유
     */
    public enum FinishMatchReason {
        LAST_QUESTION_DONE,  // 정상 종료: 3라운드 완료
        PLAYER_LEFT,         // 조기 종료: 플레이어 이탈
        HEARTBEAT_TIMEOUT    // 조기 종료: 하트비트 타임아웃
    }

    /**
     * 종료 결과
     */
    public static class MatchFinishResult {
        private final boolean processed;
        private final boolean alreadyFinished;
        private final String winner;
        private final boolean xpGranted;

        private MatchFinishResult(boolean processed, boolean alreadyFinished, String winner, boolean xpGranted) {
            this.processed = processed;
            this.alreadyFinished = alreadyFinished;
            this.winner = winner;
            this.xpGranted = xpGranted;
        }

        public static MatchFinishResult skipped() {
            return new MatchFinishResult(false, false, null, false);
        }

        public static MatchFinishResult alreadyFinished() {
            return new MatchFinishResult(false, true, null, false);
        }

        public static MatchFinishResult completed(String winner, boolean xpGranted) {
            return new MatchFinishResult(true, false, winner, xpGranted);
        }

        public boolean isProcessed() {
            return processed;
        }

        public boolean isAlreadyFinished() {
            return alreadyFinished;
        }

        public String getWinner() {
            return winner;
        }

        public boolean isXpGranted() {
            return xpGranted;
        }
    }
}



