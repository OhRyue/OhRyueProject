package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import com.OhRyue.certpilot.versus.repository.MatchRoomRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;

/**
 * GOLDENBELL 예약 시스템 스케줄러
 * - 10분 전부터 입장 가능
 * - 예약 시간이 되면 자동 시작
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class GoldenbellSchedulerService {

    private final MatchRoomRepository roomRepository;
    private final VersusService versusService;

    /**
     * 매 1분마다 실행: 예약 시간이 된 GOLDENBELL 방 자동 시작
     */
    @Scheduled(fixedRate = 60000) // 1분마다
    @Transactional
    public void autoStartScheduledGoldenbell() {
        Instant now = Instant.now();
        
        // 예약 시간이 지났고 아직 시작하지 않은 GOLDENBELL 방 찾기
        List<MatchRoom> scheduledRooms = roomRepository.findByModeAndStatusAndScheduledAtLessThanEqual(
            MatchMode.GOLDENBELL, 
            MatchStatus.WAIT, 
            now
        );

        for (MatchRoom room : scheduledRooms) {
            try {
                // 예약 시간이 지났으므로 자동 시작
                // validateMinParticipants에서 이미 예약 시스템은 최소 인원 체크 안 함
                log.info("Auto-starting scheduled GOLDENBELL room {} at scheduled time {}", 
                    room.getId(), room.getScheduledAt());
                
                versusService.startRoom(room.getId());
            } catch (Exception e) {
                log.error("Failed to auto-start scheduled GOLDENBELL room {}: {}", 
                    room.getId(), e.getMessage(), e);
            }
        }
    }

    /**
     * GOLDENBELL 방 입장 가능 여부 확인 (10분 전부터 입장 가능)
     */
    public boolean canJoinGoldenbellRoom(MatchRoom room) {
        if (room.getMode() != MatchMode.GOLDENBELL) {
            return true; // GOLDENBELL이 아니면 항상 입장 가능
        }

        if (room.getScheduledAt() == null) {
            return true; // 예약이 없으면 항상 입장 가능
        }

        Instant now = Instant.now();
        Instant tenMinutesBefore = room.getScheduledAt().minus(10, ChronoUnit.MINUTES);
        
        // 10분 전부터 입장 가능
        return now.isAfter(tenMinutesBefore) || now.equals(tenMinutesBefore);
    }

    /**
     * 매 30초마다 실행: 하트비트 타임아웃된 참가자 자동 제거
     * 대기 중인 방에서 1분 이상 하트비트가 없는 참가자를 제거
     */
    @Scheduled(fixedRate = 30000) // 30초마다
    @Transactional
    public void removeTimeoutParticipants() {
        try {
            int removedCount = versusService.removeTimeoutParticipants();
            if (removedCount > 0) {
                log.info("Removed {} timeout participants from waiting rooms", removedCount);
            }
        } catch (Exception e) {
            log.error("Failed to remove timeout participants: {}", e.getMessage(), e);
        }
    }
}

