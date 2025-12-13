package com.OhRyue.certpilot.versus.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.UUID;
import java.util.function.Supplier;

/**
 * Redis 기반 분산락 서비스
 * 
 * 여러 인스턴스 환경에서 동일한 작업이 중복 실행되는 것을 방지
 * SET NX PX 방식을 사용하여 락 획득 및 자동 만료 처리
 * 
 * 사용 예시:
 * <pre>
 * redisLockService.executeWithLock("room:123", 30, () -> {
 *     // 락이 획득된 경우에만 실행되는 로직
 *     processNextQuestion(roomId);
 *     return null;
 * });
 * </pre>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RedisLockService {

    private static final String LOCK_PREFIX = "lock:versus:room:";
    private static final int DEFAULT_LOCK_TIMEOUT_SECONDS = 30;

    private final StringRedisTemplate redisTemplate;

    /**
     * 분산락을 획득하여 작업 실행
     * 
     * @param roomId 방 ID
     * @param timeoutSeconds 락 타임아웃 (초)
     * @param task 실행할 작업
     * @param <T> 반환 타입
     * @return 작업 결과 (락 획득 실패 시 null)
     */
    public <T> T executeWithLock(Long roomId, int timeoutSeconds, Supplier<T> task) {
        String lockKey = getLockKey(roomId);
        String lockValue = UUID.randomUUID().toString();

        try {
            // SET NX PX: 키가 없으면 설정하고, 만료 시간 설정
            Boolean acquired = redisTemplate.opsForValue().setIfAbsent(
                    lockKey,
                    lockValue,
                    Duration.ofSeconds(timeoutSeconds)
            );

            if (Boolean.TRUE.equals(acquired)) {
                // 락 획득 성공
                log.debug("Lock acquired: roomId={}, lockKey={}, timeout={}s", 
                        roomId, lockKey, timeoutSeconds);
                try {
                    return task.get();
                } finally {
                    // 락 해제 (자신이 획득한 락인지 확인 후 해제)
                    releaseLock(lockKey, lockValue);
                }
            } else {
                // 락 획득 실패 (다른 인스턴스가 이미 처리 중)
                log.debug("Lock acquisition failed: roomId={}, lockKey={} (already locked by another instance)", 
                        roomId, lockKey);
                return null;
            }
        } catch (Exception e) {
            log.error("Error acquiring lock: roomId={}, lockKey={}, error={}", 
                    roomId, lockKey, e.getMessage(), e);
            // 락 획득 실패 시에도 락 해제 시도
            try {
                releaseLock(lockKey, lockValue);
            } catch (Exception releaseError) {
                log.warn("Error releasing lock: roomId={}, lockKey={}, error={}", 
                        roomId, lockKey, releaseError.getMessage());
            }
            return null;
        }
    }

    /**
     * 분산락을 획득하여 작업 실행 (반환값 없음)
     * 
     * @param roomId 방 ID
     * @param timeoutSeconds 락 타임아웃 (초)
     * @param task 실행할 작업
     */
    public void executeWithLock(Long roomId, int timeoutSeconds, Runnable task) {
        executeWithLock(roomId, timeoutSeconds, () -> {
            task.run();
            return null;
        });
    }

    /**
     * 분산락을 획득하여 작업 실행 (기본 타임아웃 사용)
     * 
     * @param roomId 방 ID
     * @param task 실행할 작업
     * @param <T> 반환 타입
     * @return 작업 결과 (락 획득 실패 시 null)
     */
    public <T> T executeWithLock(Long roomId, Supplier<T> task) {
        return executeWithLock(roomId, DEFAULT_LOCK_TIMEOUT_SECONDS, task);
    }

    /**
     * 분산락을 획득하여 작업 실행 (기본 타임아웃 사용, 반환값 없음)
     * 
     * @param roomId 방 ID
     * @param task 실행할 작업
     */
    public void executeWithLock(Long roomId, Runnable task) {
        executeWithLock(roomId, DEFAULT_LOCK_TIMEOUT_SECONDS, task);
    }

    /**
     * 락 해제
     * 
     * Lua 스크립트를 사용하여 자신이 획득한 락인지 확인 후 해제
     * (다른 인스턴스가 만료 시간 연장한 경우를 방지)
     */
    private void releaseLock(String lockKey, String lockValue) {
        try {
            // Lua 스크립트: 값이 일치할 때만 삭제
            String luaScript = 
                "if redis.call('get', KEYS[1]) == ARGV[1] then " +
                "    return redis.call('del', KEYS[1]) " +
                "else " +
                "    return 0 " +
                "end";
            
            Long result = redisTemplate.execute(
                    new org.springframework.data.redis.core.script.DefaultRedisScript<>(luaScript, Long.class),
                    java.util.Collections.singletonList(lockKey),
                    lockValue
            );

            if (result != null && result > 0) {
                log.debug("Lock released: lockKey={}", lockKey);
            } else {
                log.debug("Lock release skipped: lockKey={} (lock value mismatch or already expired)", lockKey);
            }
        } catch (Exception e) {
            log.warn("Error releasing lock: lockKey={}, error={}", lockKey, e.getMessage());
        }
    }

    /**
     * 락 키 생성
     */
    private String getLockKey(Long roomId) {
        return LOCK_PREFIX + roomId;
    }

    /**
     * 락이 현재 획득되어 있는지 확인 (디버깅용)
     * 
     * @param roomId 방 ID
     * @return 락이 획득되어 있으면 true
     */
    public boolean isLocked(Long roomId) {
        String lockKey = getLockKey(roomId);
        return Boolean.TRUE.equals(redisTemplate.hasKey(lockKey));
    }
}


