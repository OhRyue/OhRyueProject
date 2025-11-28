package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.dto.MatchingDtos;
import com.OhRyue.certpilot.versus.dto.VersusDtos;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 시연용 매칭 서비스
 * - 더미 플레이어 자동 생성
 * - 시연 모드에서 자동으로 매칭 완료
 */
@Slf4j
@Service
public class DemoMatchingService {

    private final MatchingQueueService matchingQueueService;
    private final boolean demoModeEnabled;

    public DemoMatchingService(
        MatchingQueueService matchingQueueService,
        @Value("${versus.demo-mode:false}") boolean demoModeEnabled
    ) {
        this.matchingQueueService = matchingQueueService;
        this.demoModeEnabled = demoModeEnabled;
    }

    /**
     * 시연용 매칭 요청 (더미 플레이어 자동 생성)
     */
    public MatchingDtos.MatchStatusResp requestMatchWithDemo(String userId, MatchingDtos.MatchRequest request) {
        // 실제 사용자 매칭 요청
        MatchingDtos.MatchStatusResp response = matchingQueueService.requestMatch(userId, request);
        
        // 시연 모드가 활성화되어 있고, 더미 플레이어가 필요한 경우
        if (demoModeEnabled && response.waitingCount() < getRequiredCount(request.mode())) {
            int needed = getRequiredCount(request.mode()) - response.waitingCount();
            log.info("시연 모드: 더미 플레이어 {}명 자동 생성", needed);
            
            // 더미 플레이어 생성 및 매칭 요청
            for (int i = 0; i < needed; i++) {
                String dummyUserId = generateDummyUserId(i);
                try {
                    matchingQueueService.requestMatch(dummyUserId, request);
                    log.debug("더미 플레이어 생성: {}", dummyUserId);
                } catch (Exception e) {
                    log.warn("더미 플레이어 생성 실패: {}", e.getMessage());
                }
            }
            
            // 매칭 상태 다시 조회
            return matchingQueueService.getMatchStatus(userId);
        }
        
        return response;
    }

    /**
     * 시연용 즉시 매칭 (더미 플레이어로 즉시 매칭 완료)
     */
    public MatchingDtos.MatchStatusResp instantMatchWithDemo(String userId, MatchingDtos.MatchRequest request) {
        int requiredCount = getRequiredCount(request.mode());
        
        // 더미 플레이어 생성
        List<String> dummyPlayers = new ArrayList<>();
        for (int i = 0; i < requiredCount - 1; i++) {
            dummyPlayers.add(generateDummyUserId(i));
        }
        
        log.info("시연 모드: 즉시 매칭 - 실제 사용자 1명 + 더미 플레이어 {}명", dummyPlayers.size());
        
        // 실제 사용자 매칭 요청
        matchingQueueService.requestMatch(userId, request);
        
        // 더미 플레이어들 매칭 요청
        for (String dummyUserId : dummyPlayers) {
            try {
                matchingQueueService.requestMatch(dummyUserId, request);
            } catch (Exception e) {
                log.warn("더미 플레이어 매칭 요청 실패: {}", e.getMessage());
            }
        }
        
        // 매칭 상태 조회
        return matchingQueueService.getMatchStatus(userId);
    }

    /**
     * 더미 플레이어 ID 생성
     */
    private String generateDummyUserId(int index) {
        return "demo-player-" + UUID.randomUUID().toString().substring(0, 8) + "-" + index;
    }

    /**
     * 모드별 필요 인원 수
     */
    private int getRequiredCount(MatchMode mode) {
        return mode == MatchMode.DUEL ? 2 : 8;
    }
}



