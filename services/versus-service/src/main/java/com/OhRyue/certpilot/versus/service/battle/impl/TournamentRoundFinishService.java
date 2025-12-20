package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchParticipant;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
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
 * TOURNAMENT 모드 라운드 종료 후처리 서비스 (단일 진입점)
 * 
 * 라운드 종료 시 탈락 처리 및 다음 라운드 시작을 담당합니다.
 * Redis 락으로 동시성 보장, 멱등성 플래그로 재시도 방어.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TournamentRoundFinishService {

    private static final String EVENT_ROUND_COMPLETED = "ROUND_COMPLETED";
    private static final String EVENT_PLAYER_ELIMINATED = "PLAYER_ELIMINATED";
    private static final long LOCK_TTL_MS = 15000; // 15초

    // 라운드별 생존자 수: 8→4→2→1
    private static final int[] SURVIVORS_PER_ROUND = {0, 4, 2, 1}; // 인덱스 0은 사용 안 함

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final MatchAnswerRepository answerRepository;
    private final MatchEventRepository eventRepository;
    private final TournamentBracketRepository bracketRepository;
    private final RedisLockService redisLockService;
    private final VersusService versusService;
    private final RealtimeEventService realtimeEventService;
    private final ObjectMapper objectMapper;

    /**
     * 라운드 종료 후처리 (단일 진입점)
     * 
     * @param roomId 방 ID
     * @param round 라운드 번호 (1, 2, 3)
     * @return RoundFinishResult
     */
    @Transactional
    public RoundFinishResult finishRound(Long roomId, int round) {
        String lockKey = String.format("versus:lock:TOURNAMENT:rfinish:%d:%d", roomId, round);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("TMT_RFINISH_LOCK_SKIPPED roomId={} round={}", roomId, round);
            return RoundFinishResult.skipped();
        }

        try {
            log.info("TMT_RFINISH_LOCK_ACQUIRED roomId={} round={}", roomId, round);

            // 2. 멱등성 방어: 이미 종료된 라운드인지 확인
            MatchRoom room = roomRepository.findById(roomId)
                    .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

            if (isRoundAlreadyFinished(roomId, round)) {
                log.info("TMT_RFINISH_ALREADY_DONE roomId={} round={}", roomId, round);
                return RoundFinishResult.alreadyFinished();
            }

            // 3. 활성 참가자 조회
            List<MatchParticipant> activeParticipants = participantRepository.findByRoomIdAndEliminatedFalse(roomId);
            if (activeParticipants.isEmpty()) {
                log.warn("No active participants for round finish: roomId={}, round={}", roomId, round);
                return RoundFinishResult.skipped();
            }

            // 4. 스코어보드 계산 (활성 참가자만)
            VersusDtos.ScoreBoardResp scoreboard = versusService.computeScoreboard(room);
            List<VersusDtos.ScoreBoardItem> ordered = scoreboard.items().stream()
                    .filter(item -> activeParticipants.stream()
                            .anyMatch(p -> p.getUserId().equals(item.userId())))
                    .sorted((a, b) -> {
                        // 1순위: 점수 (내림차순)
                        int scoreCompare = Integer.compare(b.score(), a.score());
                        if (scoreCompare != 0) return scoreCompare;
                        
                        // 2순위: 총 제출속도 합산 (오름차순, 빠른 사람이 우위)
                        long timeA = Optional.ofNullable(a.totalTimeMs()).orElse(0L);
                        long timeB = Optional.ofNullable(b.totalTimeMs()).orElse(0L);
                        int timeCompare = Long.compare(timeA, timeB);
                        if (timeCompare != 0) return timeCompare;
                        
                        // 3순위: 마지막 라운드 제출속도 (오름차순)
                        // TODO: 마지막 라운드 제출속도 계산
                        
                        // 4순위: userId 오름차순 (deterministic tie-breaker)
                        return a.userId().compareTo(b.userId());
                    })
                    .collect(Collectors.toList());

            // 5. 생존자 수 결정
            int targetSurvivors = SURVIVORS_PER_ROUND[round];
            if (targetSurvivors <= 0 || targetSurvivors > activeParticipants.size()) {
                // 마지막 라운드이거나 이미 목표 생존자 수 이하
                targetSurvivors = Math.max(1, activeParticipants.size());
            }

            // 6. 생존자 및 탈락자 선정
            List<String> survivors = ordered.stream()
                    .limit(targetSurvivors)
                    .map(VersusDtos.ScoreBoardItem::userId)
                    .collect(Collectors.toList());

            List<String> eliminatedIds = activeParticipants.stream()
                    .map(MatchParticipant::getUserId)
                    .filter(id -> !survivors.contains(id))
                    .collect(Collectors.toList());

            log.info("TMT_RFINISH_SURVIVORS roomId={} round={} activeParticipants={} targetSurvivors={} survivors={} eliminated={}",
                    roomId, round, activeParticipants.size(), targetSurvivors, survivors, eliminatedIds);

            // 7. 탈락 처리
            if (!eliminatedIds.isEmpty()) {
                List<MatchParticipant> toUpdate = activeParticipants.stream()
                        .filter(p -> eliminatedIds.contains(p.getUserId()))
                        .peek(p -> p.setEliminated(true))
                        .toList();
                participantRepository.saveAll(toUpdate);

                // 탈락 이벤트 기록
                for (String userId : eliminatedIds) {
                    recordEvent(roomId, EVENT_PLAYER_ELIMINATED, Map.of(
                            "userId", userId,
                            "mode", "TOURNAMENT",
                            "round", round,
                            "eliminatedAt", Instant.now().toString()
                    ));
                }
            }

            // 8. 라운드 완료 이벤트 기록
            recordEvent(roomId, EVENT_ROUND_COMPLETED, Map.of(
                    "mode", "TOURNAMENT",
                    "round", round,
                    "survivors", survivors,
                    "eliminated", eliminatedIds,
                    "completedAt", Instant.now().toString()
            ));

            // 9. 브래킷 정보 저장
            persistBracket(roomId, round, survivors, eliminatedIds);

            log.info("TMT_RFINISH_DONE roomId={} round={} survivors={} eliminated={}",
                    roomId, round, survivors.size(), eliminatedIds.size());

            return RoundFinishResult.completed(survivors, eliminatedIds);

        } catch (Exception e) {
            log.error("TMT_RFINISH_ERROR roomId={} round={} ex={}", roomId, round, e.getMessage(), e);
            throw e;
        } finally {
            // 10. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 멱등성 방어: 이미 종료된 라운드인지 확인
     */
    private boolean isRoundAlreadyFinished(Long roomId, int round) {
        List<MatchEvent> roundCompletedEvents = eventRepository.findByRoomIdAndEventType(roomId, EVENT_ROUND_COMPLETED);
        return roundCompletedEvents.stream()
                .anyMatch(event -> {
                    try {
                        if (event.getPayloadJson() == null) return false;
                        Map<String, Object> payload = objectMapper.readValue(
                                event.getPayloadJson(), new TypeReference<Map<String, Object>>() {});
                        Object roundObj = payload.get("round");
                        return roundObj != null && round == Integer.valueOf(roundObj.toString());
                    } catch (Exception e) {
                        return false;
                    }
                });
    }

    /**
     * 브래킷 정보 저장
     */
    private void persistBracket(Long roomId, int round, List<String> survivors, List<String> eliminated) {
        try {
            Map<String, Object> pairing = Map.of(
                    "survivors", survivors,
                    "eliminated", eliminated,
                    "round", round
            );
            String pairingJson = objectMapper.writeValueAsString(pairing);

            com.OhRyue.certpilot.versus.domain.TournamentBracket bracket = 
                    com.OhRyue.certpilot.versus.domain.TournamentBracket.builder()
                            .roomId(roomId)
                            .roundNo(round)
                            .pairingJson(pairingJson)
                            .build();
            bracketRepository.save(bracket);
        } catch (Exception e) {
            log.warn("Failed to persist bracket: roomId={}, round={}, error={}",
                    roomId, round, e.getMessage());
        }
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
     * 라운드 종료 결과
     */
    public static class RoundFinishResult {
        private final boolean processed;
        private final boolean skipped;
        private final boolean alreadyFinished;
        private final List<String> survivors;
        private final List<String> eliminated;

        private RoundFinishResult(boolean processed, boolean skipped, boolean alreadyFinished, 
                                 List<String> survivors, List<String> eliminated) {
            this.processed = processed;
            this.skipped = skipped;
            this.alreadyFinished = alreadyFinished;
            this.survivors = survivors;
            this.eliminated = eliminated;
        }

        public static RoundFinishResult skipped() {
            return new RoundFinishResult(false, true, false, List.of(), List.of());
        }

        public static RoundFinishResult alreadyFinished() {
            return new RoundFinishResult(false, false, true, List.of(), List.of());
        }

        public static RoundFinishResult completed(List<String> survivors, List<String> eliminated) {
            return new RoundFinishResult(true, false, false, survivors, eliminated);
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

        public List<String> getSurvivors() {
            return survivors;
        }

        public List<String> getEliminated() {
            return eliminated;
        }
    }
}








