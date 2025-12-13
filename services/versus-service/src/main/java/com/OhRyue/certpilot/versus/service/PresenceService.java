package com.OhRyue.certpilot.versus.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Redis 기반 Presence 서비스
 * 
 * WebSocket 연결 상태를 Redis에 저장하여 멀티 인스턴스 환경에서도
 * 모든 인스턴스가 참가자의 접속 상태를 확인할 수 있도록 함
 * 
 * 키 구조: presence:room:{roomId}
 * 값: Hash { userId -> lastSeenAt (ISO-8601 문자열) }
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class PresenceService {

    private static final String PRESENCE_PREFIX = "presence:room:";
    private static final int PRESENCE_TTL_SECONDS = 300; // 5분 TTL (하트비트가 없으면 자동 삭제)

    private final StringRedisTemplate redisTemplate;

    /**
     * 하트비트 업데이트 (접속 상태 갱신)
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     */
    public void updatePresence(Long roomId, String userId) {
        String key = getPresenceKey(roomId);
        String lastSeenAt = Instant.now().toString();

        try {
            // Hash에 userId -> lastSeenAt 저장
            redisTemplate.opsForHash().put(key, userId, lastSeenAt);
            
            // 키에 TTL 설정 (하트비트가 없으면 자동 삭제)
            redisTemplate.expire(key, Duration.ofSeconds(PRESENCE_TTL_SECONDS));

            log.debug("Presence updated: roomId={}, userId={}, lastSeenAt={}", roomId, userId, lastSeenAt);
        } catch (Exception e) {
            log.warn("Failed to update presence: roomId={}, userId={}, error={}", 
                    roomId, userId, e.getMessage());
        }
    }

    /**
     * 특정 사용자의 마지막 접속 시간 조회
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     * @return 마지막 접속 시간 (없으면 null)
     */
    public Instant getLastSeenAt(Long roomId, String userId) {
        String key = getPresenceKey(roomId);
        
        try {
            Object value = redisTemplate.opsForHash().get(key, userId);
            if (value == null) {
                return null;
            }
            return Instant.parse(value.toString());
        } catch (Exception e) {
            log.warn("Failed to get lastSeenAt: roomId={}, userId={}, error={}", 
                    roomId, userId, e.getMessage());
            return null;
        }
    }

    /**
     * 방의 모든 접속 중인 사용자 목록 조회
     * 
     * @param roomId 방 ID
     * @return 접속 중인 사용자 ID Set
     */
    public Set<String> getActiveUsers(Long roomId) {
        String key = getPresenceKey(roomId);
        
        try {
            Map<Object, Object> presenceMap = redisTemplate.opsForHash().entries(key);
            return presenceMap.keySet().stream()
                    .map(Object::toString)
                    .collect(Collectors.toSet());
        } catch (Exception e) {
            log.warn("Failed to get active users: roomId={}, error={}", roomId, e.getMessage());
            return Set.of();
        }
    }

    /**
     * 사용자 접속 상태 제거 (연결 해제 시)
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     */
    public void removePresence(Long roomId, String userId) {
        String key = getPresenceKey(roomId);
        
        try {
            redisTemplate.opsForHash().delete(key, userId);
            log.debug("Presence removed: roomId={}, userId={}", roomId, userId);
        } catch (Exception e) {
            log.warn("Failed to remove presence: roomId={}, userId={}, error={}", 
                    roomId, userId, e.getMessage());
        }
    }

    /**
     * 타임아웃된 사용자 목록 조회
     * 
     * @param roomId 방 ID
     * @param timeoutSeconds 타임아웃 시간 (초)
     * @return 타임아웃된 사용자 ID Set
     */
    public Set<String> getTimeoutUsers(Long roomId, int timeoutSeconds) {
        String key = getPresenceKey(roomId);
        Instant thresholdTime = Instant.now().minusSeconds(timeoutSeconds);
        
        try {
            Map<Object, Object> presenceMap = redisTemplate.opsForHash().entries(key);
            return presenceMap.entrySet().stream()
                    .filter(entry -> {
                        try {
                            Instant lastSeenAt = Instant.parse(entry.getValue().toString());
                            return lastSeenAt.isBefore(thresholdTime);
                        } catch (Exception e) {
                            // 파싱 실패 시 타임아웃으로 간주
                            return true;
                        }
                    })
                    .map(entry -> entry.getKey().toString())
                    .collect(Collectors.toSet());
        } catch (Exception e) {
            log.warn("Failed to get timeout users: roomId={}, error={}", roomId, e.getMessage());
            return Set.of();
        }
    }

    /**
     * Presence 키 생성
     */
    private String getPresenceKey(Long roomId) {
        return PRESENCE_PREFIX + roomId;
    }
}


