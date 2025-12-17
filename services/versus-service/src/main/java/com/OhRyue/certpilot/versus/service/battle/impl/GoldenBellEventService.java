package com.OhRyue.certpilot.versus.service.battle.impl;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.service.RedisLockService;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.OhRyue.certpilot.versus.service.VersusService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * GOLDENBELL 이벤트 서비스
 * 
 * 자동 시작 및 입장 윈도우 관리를 담당합니다.
 * - 시작 10분 전 WAITING_OPEN 상태 생성
 * - 시작 시각에 LOCK + IN_PROGRESS 전환
 * - 입장 인원 제한 (최대 20명)
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GoldenBellEventService {

    private static final int MAX_PLAYERS = 20;
    private static final int ENTRY_WINDOW_MINUTES = 10; // 시작 10분 전부터 입장 가능
    private static final long START_LOCK_TTL_MS = 180000; // 3분

    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final RedisLockService redisLockService;
    private final RealtimeEventService realtimeEventService;
    private final VersusService versusService;
    private final ObjectMapper objectMapper;

    /**
     * 골든벨 이벤트 시작 (스케줄러에서 호출)
     * 
     * @param examMode WRITTEN 또는 PRACTICAL
     * @param scheduledAt 시작 시각
     * @return 생성된 방 ID
     */
    @Transactional
    public Long startGoldenBellEvent(String examMode, Instant scheduledAt) {
        // 락 키 생성: versus:lock:goldenbell:start:{examMode}:{yyyyMMddHHmm}
        String slot = scheduledAt.atZone(ZoneId.systemDefault())
                .format(DateTimeFormatter.ofPattern("yyyyMMddHHmm"));
        String lockKey = String.format("versus:lock:goldenbell:start:%s:%s", examMode, slot);
        String requestId = java.util.UUID.randomUUID().toString();
        Duration ttl = Duration.ofMillis(START_LOCK_TTL_MS);

        // 1. 락 획득 시도 (멀티 인스턴스 대비)
        boolean lockAcquired = redisLockService.tryLock(lockKey, requestId, ttl);
        if (!lockAcquired) {
            log.debug("GB_START_LOCK_SKIPPED examMode={} slot={}", examMode, slot);
            return null; // 다른 인스턴스가 이미 처리 중
        }

        try {
            log.info("GB_START_LOCK_ACQUIRED examMode={} slot={}", examMode, slot);

            // 2. 이미 생성된 방이 있는지 확인
            List<MatchRoom> existingRooms = roomRepository.findByModeAndStatus(
                    MatchMode.GOLDENBELL, MatchStatus.WAIT);
            // TODO: scheduledAt 기준으로 필터링하여 중복 생성 방지

            // 3. WAITING_OPEN 상태의 방 생성 (시작 10분 전)
            Instant openAt = scheduledAt.minus(Duration.ofMinutes(ENTRY_WINDOW_MINUTES));
            
            // TODO: 실제 방 생성 로직 (기존 VersusService의 방 생성 로직 활용)
            // 현재는 스캐폴딩만 제공
            MatchRoom room = MatchRoom.builder()
                    .mode(MatchMode.GOLDENBELL)
                    .status(MatchStatus.WAIT)
                    .scheduledAt(scheduledAt)
                    .build();
            // scopeJson에 examMode 저장
            Map<String, Object> scope = new HashMap<>();
            scope.put("examMode", examMode);
            try {
                room.setScopeJson(objectMapper.writeValueAsString(scope));
            } catch (Exception e) {
                log.warn("Failed to set scopeJson: {}", e.getMessage());
            }

            MatchRoom savedRoom = roomRepository.save(room);

            // 4. GB_WAITING_OPEN 이벤트 기록
            recordWaitingOpen(savedRoom.getId(), openAt, scheduledAt);

            log.info("GB_WAITING_OPEN roomId={} examMode={} openAt={} startAt={}", 
                    savedRoom.getId(), examMode, openAt, scheduledAt);

            return savedRoom.getId();

        } catch (Exception e) {
            log.error("GB_START_ERROR examMode={} slot={} ex={}", 
                    examMode, slot, e.getMessage(), e);
            throw e;
        } finally {
            // 5. 락 해제
            redisLockService.unlock(lockKey, requestId);
        }
    }

    /**
     * 골든벨 시작 (시작 시각에 호출)
     * 
     * @param roomId 방 ID
     */
    @Transactional
    public void lockAndStart(Long roomId) {
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

        if (room.getMode() != MatchMode.GOLDENBELL) {
            log.warn("GoldenBellEventService.lockAndStart called for non-GOLDENBELL room: roomId={}, mode={}", 
                    roomId, room.getMode());
            return;
        }

        // 1. 방 상태를 LOCKED로 전환
        room.setStatus(MatchStatus.ONGOING);
        roomRepository.save(room);

        // 2. GB_LOCKED 이벤트 기록
        recordLocked(roomId);

        log.info("GB_LOCKED roomId={}", roomId);

        // 3. 첫 문제 시작 (기존 VersusService의 문제 시작 로직 활용)
        // TODO: 첫 문제 시작 처리
    }

    /**
     * 골든벨 입장 처리
     * 
     * @param roomId 방 ID
     * @param userId 사용자 ID
     * @return 입장 성공 여부
     */
    @Transactional
    public boolean joinGoldenBell(Long roomId, String userId) {
        MatchRoom room = roomRepository.findById(roomId)
                .orElseThrow(() -> new IllegalStateException("Room not found: " + roomId));

        if (room.getMode() != MatchMode.GOLDENBELL) {
            log.warn("joinGoldenBell called for non-GOLDENBELL room: roomId={}, mode={}", 
                    roomId, room.getMode());
            return false;
        }

        // 1. 시작 후 입장 불가 확인
        if (room.getStatus() != MatchStatus.WAIT) {
            log.warn("Cannot join GOLDENBELL room after start: roomId={}, status={}", 
                    roomId, room.getStatus());
            return false;
        }

        // 2. 최대 인원 확인
        long currentCount = participantRepository.countByRoomId(roomId);
        if (currentCount >= MAX_PLAYERS) {
            log.warn("GOLDENBELL room is full: roomId={}, currentCount={}, max={}", 
                    roomId, currentCount, MAX_PLAYERS);
            return false;
        }

        // 3. 중복 입장 확인
        boolean alreadyJoined = participantRepository.findByRoomId(roomId).stream()
                .anyMatch(p -> p.getUserId().equals(userId));
        if (alreadyJoined) {
            log.debug("User already joined: roomId={}, userId={}", roomId, userId);
            return true; // 이미 입장했으면 성공으로 처리
        }

        // 4. 참가자 추가
        // TODO: 실제 참가자 추가 로직 (기존 VersusService의 참가자 추가 로직 활용)

        // 5. GB_JOINED 이벤트 기록
        recordJoined(roomId, userId, currentCount + 1);

        log.info("GB_JOINED roomId={} userId={} currentCount={}", roomId, userId, currentCount + 1);

        return true;
    }

    /**
     * GB_WAITING_OPEN 이벤트 기록
     */
    private void recordWaitingOpen(Long roomId, Instant openAt, Instant startAt) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("roomId", roomId);
        payload.put("openAt", openAt.toString());
        payload.put("startAt", startAt.toString());
        payload.put("remainMs", Duration.between(Instant.now(), startAt).toMillis());

        try {
            String payloadJson = objectMapper.writeValueAsString(payload);
            // TODO: MatchEvent 저장 및 브로드캐스트
        } catch (Exception e) {
            log.warn("Failed to record GB_WAITING_OPEN: roomId={}, error={}", roomId, e.getMessage());
        }
    }

    /**
     * GB_LOCKED 이벤트 기록
     */
    private void recordLocked(Long roomId) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("roomId", roomId);
        payload.put("lockedAt", Instant.now().toString());

        try {
            String payloadJson = objectMapper.writeValueAsString(payload);
            // TODO: MatchEvent 저장 및 브로드캐스트
        } catch (Exception e) {
            log.warn("Failed to record GB_LOCKED: roomId={}, error={}", roomId, e.getMessage());
        }
    }

    /**
     * GB_JOINED 이벤트 기록
     */
    private void recordJoined(Long roomId, String userId, long currentCount) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("roomId", roomId);
        payload.put("userId", userId);
        payload.put("currentCount", currentCount);
        payload.put("maxCount", MAX_PLAYERS);
        payload.put("joinedAt", Instant.now().toString());

        try {
            String payloadJson = objectMapper.writeValueAsString(payload);
            // TODO: MatchEvent 저장 및 브로드캐스트
        } catch (Exception e) {
            log.warn("Failed to record GB_JOINED: roomId={}, userId={}, error={}", 
                    roomId, userId, e.getMessage());
        }
    }
}




