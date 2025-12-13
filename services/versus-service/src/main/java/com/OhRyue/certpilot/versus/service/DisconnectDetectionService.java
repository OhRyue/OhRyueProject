package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.GoldenbellState;
import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchParticipant;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.repository.GoldenbellStateRepository;
import com.OhRyue.certpilot.versus.repository.MatchEventRepository;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import com.OhRyue.certpilot.versus.service.RealtimeEventService;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 이탈 감지 및 처리 서비스
 * 
 * Redis Presence를 기반으로 사용자의 접속 상태를 확인하고,
 * 일정 시간 이상 미응답 시 게임 모드에 맞게 처리
 * 
 * 처리 정책:
 * - DUEL: 유예 후 기권 처리
 * - TOURNAMENT / GOLDENBELL: 탈락 처리
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DisconnectDetectionService {

    private static final int HEARTBEAT_TIMEOUT_SECONDS = 30; // 하트비트 타임아웃 시간 (초)
    private static final int DUEL_GRACE_PERIOD_SECONDS = 60; // DUEL 모드 유예 시간 (초)

    private final PresenceService presenceService;
    private final RedisLockService redisLockService;
    private final MatchRoomRepository roomRepository;
    private final MatchParticipantRepository participantRepository;
    private final GoldenbellStateRepository goldenbellStateRepository;
    private final MatchEventRepository eventRepository;
    private final RealtimeEventService realtimeEventService;
    private final VersusService versusService;
    private final ObjectMapper objectMapper;

    /**
     * 매 10초마다 실행: 이탈 감지 및 처리
     * 
     * 분산락을 사용하여 여러 인스턴스에서 중복 실행 방지
     */
    @Scheduled(fixedRate = 10000) // 10초마다
    public void detectAndHandleDisconnects() {
        // 진행 중인 방 찾기
        List<MatchRoom> ongoingRooms = roomRepository.findByStatus(MatchStatus.ONGOING);

        for (MatchRoom room : ongoingRooms) {
            try {
                // 분산락을 획득한 경우에만 처리
                redisLockService.executeWithLock(room.getId(), 30, () -> {
                    processRoomDisconnects(room);
                    return null;
                });
            } catch (Exception e) {
                log.error("Failed to process disconnects for room {}: {}", 
                    room.getId(), e.getMessage(), e);
            }
        }
    }

    /**
     * 특정 방의 이탈 감지 및 처리
     * 
     * 분산락으로 보호되어 있어 여러 인스턴스에서 중복 실행되지 않음
     */
    @Transactional
    private void processRoomDisconnects(MatchRoom room) {
        Long roomId = room.getId();
        
        // 방의 모든 참가자 조회
        List<MatchParticipant> participants = participantRepository.findByRoomId(roomId);
        
        // Redis에서 타임아웃된 사용자 목록 조회
        Set<String> timeoutUsers = presenceService.getTimeoutUsers(roomId, HEARTBEAT_TIMEOUT_SECONDS);
        
        if (timeoutUsers.isEmpty()) {
            return; // 타임아웃된 사용자 없음
        }

        log.info("Detected timeout users in room {}: {}", roomId, timeoutUsers);

        // 봇은 제외
        timeoutUsers.removeIf(userId -> userId.startsWith("BOT_"));

        if (timeoutUsers.isEmpty()) {
            return; // 타임아웃된 실제 사용자 없음
        }

        // 모드별 처리
        switch (room.getMode()) {
            case DUEL -> handleDuelDisconnect(room, participants, timeoutUsers);
            case TOURNAMENT -> handleTournamentDisconnect(room, participants, timeoutUsers);
            case GOLDENBELL -> handleGoldenbellDisconnect(room, participants, timeoutUsers);
        }
    }

    /**
     * DUEL 모드 이탈 처리
     * 
     * 유예 시간(60초) 후 기권 처리
     */
    private void handleDuelDisconnect(MatchRoom room, List<MatchParticipant> participants, Set<String> timeoutUsers) {
        Long roomId = room.getId();
        Instant now = Instant.now();

        for (String userId : timeoutUsers) {
            MatchParticipant participant = participants.stream()
                    .filter(p -> p.getUserId().equals(userId))
                    .findFirst()
                    .orElse(null);

            if (participant == null) {
                continue;
            }

            // 유예 시간 확인
            Instant lastHeartbeatAt = participant.getLastHeartbeatAt();
            if (lastHeartbeatAt == null) {
                lastHeartbeatAt = participant.getJoinedAt();
            }

            long secondsSinceLastHeartbeat = now.getEpochSecond() - lastHeartbeatAt.getEpochSecond();

            if (secondsSinceLastHeartbeat < DUEL_GRACE_PERIOD_SECONDS) {
                // 아직 유예 시간 내
                log.debug("DUEL disconnect grace period: roomId={}, userId={}, secondsSinceLastHeartbeat={}",
                        roomId, userId, secondsSinceLastHeartbeat);
                continue;
            }

            // 유예 시간 경과: 기권 처리
            log.info("DUEL disconnect timeout: roomId={}, userId={}, secondsSinceLastHeartbeat={}",
                    roomId, userId, secondsSinceLastHeartbeat);

            // 이벤트 기록
            recordEvent(roomId, "PLAYER_DISCONNECTED", Map.of(
                    "userId", userId,
                    "disconnectedAt", now.toString(),
                    "reason", "HEARTBEAT_TIMEOUT",
                    "mode", "DUEL",
                    "gracePeriodSeconds", DUEL_GRACE_PERIOD_SECONDS
            ));

            // 참가자 제거
            participantRepository.delete(participant);

            // 남은 참가자 확인
            long remainingParticipants = participantRepository.countByRoomId(roomId);

            if (remainingParticipants <= 1) {
                // 게임 종료 처리
                room.setStatus(MatchStatus.DONE);
                roomRepository.save(room);

                // 승자 결정
                String winner = null;
                if (remainingParticipants == 1) {
                    List<MatchParticipant> remaining = participantRepository.findByRoomId(roomId);
                    if (!remaining.isEmpty()) {
                        winner = remaining.get(0).getUserId();
                    }
                }

                recordEvent(roomId, "MATCH_FINISHED", Map.of(
                        "mode", "DUEL",
                        "winner", winner != null ? winner : "NONE",
                        "reason", "OPPONENT_DISCONNECTED",
                        "disconnectedUserId", userId,
                        "finishedAt", now.toString()
                ));

                // 게임 종료 이벤트는 기록되었으므로, 실제 보상 지급은 게임 종료 로직에서 처리
                // (notifyProgressService는 VersusService의 private 메서드이므로 여기서는 호출하지 않음)

                log.info("DUEL game ended due to disconnect: roomId={}, disconnectedUserId={}, winner={}",
                        roomId, userId, winner);
            }
        }
    }

    /**
     * TOURNAMENT 모드 이탈 처리
     * 
     * 즉시 탈락 처리
     */
    private void handleTournamentDisconnect(MatchRoom room, List<MatchParticipant> participants, Set<String> timeoutUsers) {
        Long roomId = room.getId();
        Instant now = Instant.now();

        for (String userId : timeoutUsers) {
            MatchParticipant participant = participants.stream()
                    .filter(p -> p.getUserId().equals(userId) && !p.isEliminated())
                    .findFirst()
                    .orElse(null);

            if (participant == null) {
                continue;
            }

            log.info("TOURNAMENT disconnect timeout: roomId={}, userId={}", roomId, userId);

            // 탈락 처리
            participant.setEliminated(true);
            participantRepository.save(participant);

            // 이벤트 기록
            recordEvent(roomId, "PLAYER_ELIMINATED", Map.of(
                    "userId", userId,
                    "mode", "TOURNAMENT",
                    "reason", "HEARTBEAT_TIMEOUT",
                    "eliminatedAt", now.toString()
            ));

            recordEvent(roomId, "PLAYER_DISCONNECTED", Map.of(
                    "userId", userId,
                    "disconnectedAt", now.toString(),
                    "reason", "HEARTBEAT_TIMEOUT",
                    "mode", "TOURNAMENT"
            ));
        }
    }

    /**
     * GOLDENBELL 모드 이탈 처리
     * 
     * 즉시 탈락 처리
     */
    private void handleGoldenbellDisconnect(MatchRoom room, List<MatchParticipant> participants, Set<String> timeoutUsers) {
        Long roomId = room.getId();
        Instant now = Instant.now();

        for (String userId : timeoutUsers) {
            MatchParticipant participant = participants.stream()
                    .filter(p -> p.getUserId().equals(userId))
                    .findFirst()
                    .orElse(null);

            if (participant == null) {
                continue;
            }

            // GoldenbellState 확인
            GoldenbellState goldenbellState = goldenbellStateRepository.findByRoomIdAndUserId(roomId, userId)
                    .orElse(null);
            if (goldenbellState != null && !goldenbellState.isAlive()) {
                continue; // 이미 탈락
            }

            log.info("GOLDENBELL disconnect timeout: roomId={}, userId={}", roomId, userId);

            // 탈락 처리
            participant.setEliminated(true);
            participantRepository.save(participant);

            if (goldenbellState != null) {
                goldenbellState.setAlive(false);
                goldenbellStateRepository.save(goldenbellState);
            }

            // 이벤트 기록
            recordEvent(roomId, "PLAYER_ELIMINATED", Map.of(
                    "userId", userId,
                    "mode", "GOLDENBELL",
                    "reason", "HEARTBEAT_TIMEOUT",
                    "eliminatedAt", now.toString()
            ));

            recordEvent(roomId, "PLAYER_DISCONNECTED", Map.of(
                    "userId", userId,
                    "disconnectedAt", now.toString(),
                    "reason", "HEARTBEAT_TIMEOUT",
                    "mode", "GOLDENBELL"
            ));
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
            
            // 실시간 브로드캐스트
            realtimeEventService.broadcastEvent(savedEvent);
        } catch (Exception e) {
            log.warn("Failed to record event: roomId={}, type={}, error={}", 
                    roomId, type, e.getMessage());
        }
    }
}

