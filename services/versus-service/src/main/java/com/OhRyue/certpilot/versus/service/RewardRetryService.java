package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.client.ProgressServiceClient;
import com.OhRyue.certpilot.versus.client.ProgressServiceClient.VersusResultRequest;
import com.OhRyue.certpilot.versus.config.MonitoringConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * 보상 지급 재시도 서비스
 * - progress-service 호출 실패 시 비동기로 재시도
 * - 최대 3회까지 재시도
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class RewardRetryService {

    private final ProgressServiceClient progressServiceClient;
    private final MonitoringConfig monitoringConfig;

    // 재시도 대기 큐: roomId -> (request, attemptCount, lastAttemptTime)
    private final ConcurrentMap<Long, RetryEntry> retryQueue = new ConcurrentHashMap<>();

    private static final int MAX_RETRY_ATTEMPTS = 3;
    private static final long RETRY_DELAY_MS = 5000; // 5초

    @Async
    public void retryRewardPayment(Long roomId, VersusResultRequest request) {
        RetryEntry entry = retryQueue.computeIfAbsent(roomId, 
            k -> new RetryEntry(request, 0, Instant.now()));
        
        entry.incrementAttempt();
        entry.setLastAttemptTime(Instant.now());

        try {
            progressServiceClient.recordVersusResult(request);
            
            // 성공 시 큐에서 제거
            retryQueue.remove(roomId);
            log.info("보상 지급 재시도 성공: roomId={}, attempt={}", roomId, entry.getAttemptCount());
            
        } catch (Exception e) {
            log.warn("보상 지급 재시도 실패: roomId={}, attempt={}, error={}", 
                roomId, entry.getAttemptCount(), e.getMessage());
            
            monitoringConfig.recordRewardFailure(roomId.toString(), "retry", e);
            
            // 최대 재시도 횟수 초과 시 큐에서 제거하고 로그 남김
            if (entry.getAttemptCount() >= MAX_RETRY_ATTEMPTS) {
                retryQueue.remove(roomId);
                log.error("보상 지급 재시도 최대 횟수 초과: roomId={}, request={}", 
                    roomId, request);
                // TODO: 수동 처리용 알림 발송 (예: 관리자 알림)
            }
        }
    }

    /**
     * 주기적으로 재시도 큐를 확인하고 재시도 실행
     * (5분마다 실행)
     */
    @Scheduled(fixedRate = 300000)
    public void processRetryQueue() {
        Instant now = Instant.now();
        
        retryQueue.entrySet().removeIf(entry -> {
            Long roomId = entry.getKey();
            RetryEntry retryEntry = entry.getValue();
            
            // 재시도 간격이 지났고 최대 횟수 미만이면 재시도
            if (now.isAfter(retryEntry.getLastAttemptTime().plusMillis(RETRY_DELAY_MS)) &&
                retryEntry.getAttemptCount() < MAX_RETRY_ATTEMPTS) {
                
                log.info("보상 지급 자동 재시도: roomId={}, attempt={}", 
                    roomId, retryEntry.getAttemptCount() + 1);
                
                retryRewardPayment(roomId, retryEntry.getRequest());
                return false; // 큐에서 제거하지 않음 (재시도 중)
            }
            
            // 최대 재시도 횟수 초과 시 큐에서 제거
            if (retryEntry.getAttemptCount() >= MAX_RETRY_ATTEMPTS) {
                log.error("보상 지급 재시도 최대 횟수 초과로 큐에서 제거: roomId={}", roomId);
                return true; // 큐에서 제거
            }
            
            return false; // 유지
        });
    }

    private static class RetryEntry {
        private final VersusResultRequest request;
        private int attemptCount;
        private Instant lastAttemptTime;

        public RetryEntry(VersusResultRequest request, int attemptCount, Instant lastAttemptTime) {
            this.request = request;
            this.attemptCount = attemptCount;
            this.lastAttemptTime = lastAttemptTime;
        }

        public void incrementAttempt() {
            this.attemptCount++;
        }

        public void setLastAttemptTime(Instant lastAttemptTime) {
            this.lastAttemptTime = lastAttemptTime;
        }

        public VersusResultRequest getRequest() {
            return request;
        }

        public int getAttemptCount() {
            return attemptCount;
        }

        public Instant getLastAttemptTime() {
            return lastAttemptTime;
        }
    }
}






