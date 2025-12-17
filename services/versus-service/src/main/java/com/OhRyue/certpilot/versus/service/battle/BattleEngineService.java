package com.OhRyue.certpilot.versus.service.battle;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.util.List;
import java.util.UUID;

/**
 * 배틀 공통 엔진 서비스
 * 
 * 모든 배틀 모드(DUEL/TOURNAMENT/GOLDENBELL)의 공통 진입점입니다.
 * Redis 락과 멱등성 체크를 표준화하여 동시성 안전성을 보장합니다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BattleEngineService {

    private static final long QUESTION_FINISH_LOCK_TTL_MS = 8000; // 8초
    private static final long MATCH_FINISH_LOCK_TTL_MS = 30000; // 30초

    private final MatchRoomRepository roomRepository;
    private final com.OhRyue.certpilot.versus.repository.MatchEventRepository eventRepository;
    private final RedisLockService redisLockService;
    private final List<BattleModeStrategy> strategies;
    private final com.fasterxml.jackson.databind.ObjectMapper objectMapper;

    /**
     * 모드별 Strategy 조회
     */
    private BattleModeStrategy getStrategy(MatchMode mode) {
        return strategies.stream()
                .filter(s -> s.mode() == mode)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Strategy not found for mode: " + mode));
    }

    /**
     * 질문 종료 (단일 진입점)
     * 
     * 모든 모드에서 이 메서드를 통해 질문 종료 후처리를 수행합니다.
     * Redis 락으로 동시성 보장, 멱등성 체크로 재시도 방어.
     * 
     * @param roomId 방 ID
     * @param stepKey stepKey (모드별 형식 다름)
     * @param reason 종료 사유 (SUBMIT 또는 TIMEOUT)
     * @param triggeredByUserId 트리거한 사용자 ID (TIMEOUT의 경우 null)
     * @return 질문 종료 결과
     */
    @Transactional
    public QuestionFinishResult finishQuestion(Long roomId, String stepKey, FinishReason reason, String triggeredByUserId) {
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

        BattleModeStrategy strategy = getStrategy(room.getMode());

        // 락 키 생성: versus:lock:{mode}:qfinish:{roomId}:{stepKey}
        String lockKey = String.format("versus:lock:%s:qfinish:%d:%s", 
                room.getMode().name(), roomId, stepKey);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(QUESTION_FINISH_LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("QUESTION_FINISH_LOCK_SKIPPED roomId={} stepKey={} reason={}", roomId, stepKey, reason);
            return QuestionFinishResult.skipped();
        }

        try {
            log.info("QUESTION_FINISH_LOCK_ACQUIRED roomId={} stepKey={} reason={} triggeredBy={}", 
                    roomId, stepKey, reason, triggeredByUserId);

            // 2. 멱등성 방어: 이미 종료된 질문인지 확인
            if (isQuestionAlreadyFinished(roomId, stepKey, strategy)) {
                log.info("QUESTION_FINISH_ALREADY_CLOSED roomId={} stepKey={}", roomId, stepKey);
                return QuestionFinishResult.alreadyFinished();
            }

            // 3. Strategy에서 질문 종료 처리
            QuestionFinishResult result = strategy.finishQuestion(room, stepKey, reason, triggeredByUserId);

            log.info("QUESTION_FINISH_DONE roomId={} stepKey={} nextStepKey={} matchCompleted={}", 
                    roomId, stepKey, result.getNextStepKey().orElse(null), result.isMatchCompleted());

            return result;

        } catch (Exception e) {
            log.error("QUESTION_FINISH_ERROR roomId={} stepKey={} reason={} ex={}", 
                    roomId, stepKey, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 4. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 매치 종료 (단일 진입점)
     * 
     * 모든 모드에서 이 메서드를 통해 매치 종료 후처리를 수행합니다.
     * Redis 락으로 동시성 보장, 멱등성 플래그로 재시도 방어.
     * 
     * @param roomId 방 ID
     * @param reason 종료 사유
     * @return 매치 종료 결과
     */
    @Transactional
    public MatchFinishResult finishMatch(Long roomId, FinishMatchReason reason) {
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

        BattleModeStrategy strategy = getStrategy(room.getMode());

        // 락 키 생성: versus:lock:{mode}:mfinish:{roomId}
        String lockKey = String.format("versus:lock:%s:mfinish:%d", 
                room.getMode().name(), roomId);
        String requestId = UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(MATCH_FINISH_LOCK_TTL_MS);

        // 1. 락 획득 시도
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("MATCH_FINISH_LOCK_SKIPPED roomId={} reason={}", roomId, reason);
            return MatchFinishResult.skipped();
        }

        try {
            log.info("MATCH_FINISH_LOCK_ACQUIRED roomId={} reason={}", roomId, reason);

            // 2. 멱등성 방어: 이미 종료된 매치인지 확인
            if (isMatchAlreadyFinished(room)) {
                log.info("MATCH_FINISH_ALREADY_DONE roomId={}", roomId);
                return MatchFinishResult.alreadyFinished();
            }

            // 3. Strategy에서 매치 종료 처리
            MatchFinishResult result = strategy.finishMatch(room, reason);

            log.info("MATCH_FINISH_DONE roomId={} winner={} xpGranted={}", 
                    roomId, result.getWinner(), result.isXpGranted());

            return result;

        } catch (Exception e) {
            log.error("MATCH_FINISH_ERROR roomId={} reason={} ex={}", 
                    roomId, reason, e.getMessage(), e);
            throw e;
        } finally {
            // 4. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 질문이 이미 종료 처리되었는지 확인 (멱등성 방어)
     */
    private boolean isQuestionAlreadyFinished(Long roomId, String stepKey, BattleModeStrategy strategy) {
        try {
            // MatchEventRepository에서 QUESTION_FINISHED 이벤트 확인
            List<com.OhRyue.certpilot.versus.domain.MatchEvent> finishEvents = 
                    eventRepository.findByRoomIdAndEventType(roomId, "QUESTION_FINISHED");
            
            // stepKey를 payload에서 확인
            return finishEvents.stream()
                    .anyMatch(event -> {
                        try {
                            if (event.getPayloadJson() == null) return false;
                            java.util.Map<String, Object> payload = objectMapper.readValue(
                                    event.getPayloadJson(), 
                                    new com.fasterxml.jackson.core.type.TypeReference<java.util.Map<String, Object>>() {});
                            Object stepKeyObj = payload.get("stepKey");
                            return stepKeyObj != null && stepKey.equals(stepKeyObj.toString());
                        } catch (Exception e) {
                            return false;
                        }
                    });
        } catch (Exception e) {
            log.debug("Failed to check question finished: roomId={}, stepKey={}, error={}", 
                    roomId, stepKey, e.getMessage());
            return false;
        }
    }

    /**
     * 매치가 이미 종료 처리되었는지 확인 (멱등성 방어)
     */
    private boolean isMatchAlreadyFinished(MatchRoom room) {
        // 1. 방 상태 확인
        if (room.getStatus() == com.OhRyue.certpilot.versus.domain.MatchStatus.DONE) {
            return true;
        }

        // 2. resultReported 플래그 확인
        if (room.getResultReported() != null && room.getResultReported()) {
            return true;
        }

        // 3. MATCH_FINISHED 이벤트 확인
        try {
            List<com.OhRyue.certpilot.versus.domain.MatchEvent> finishEvents = 
                    eventRepository.findByRoomIdAndEventType(room.getId(), "MATCH_FINISHED");
            return !finishEvents.isEmpty();
        } catch (Exception e) {
            log.debug("Failed to check match finished: roomId={}, error={}", room.getId(), e.getMessage());
            return false;
        }
    }

    /**
     * 종료 사유
     */
    public enum FinishReason {
        SUBMIT, TIMEOUT
    }

    /**
     * 매치 종료 사유
     */
    public enum FinishMatchReason {
        LAST_QUESTION_DONE,  // 정상 종료
        PLAYER_LEFT,         // 조기 종료: 플레이어 이탈
        HEARTBEAT_TIMEOUT    // 조기 종료: 하트비트 타임아웃
    }
}

