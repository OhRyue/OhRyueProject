package com.OhRyue.certpilot.versus.config;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * 모니터링 설정
 * - Feign Client 호출 실패 모니터링
 * - 보상 지급 실패 모니터링
 * - 응답 시간 모니터링
 */
@Slf4j
@Configuration
@EnableScheduling
@RequiredArgsConstructor
public class MonitoringConfig {

    private final MeterRegistry meterRegistry;

    // Feign Client 호출 실패 카운터
    private final ConcurrentMap<String, Long> feignFailureCounts = new ConcurrentHashMap<>();

    // 보상 지급 실패 카운터
    private final ConcurrentMap<String, Long> rewardFailureCounts = new ConcurrentHashMap<>();

    /**
     * Feign Client 호출 실패 기록
     */
    public void recordFeignFailure(String serviceName, String method, Exception e) {
        String key = serviceName + ":" + method;
        feignFailureCounts.merge(key, 1L, Long::sum);
        
        meterRegistry.counter("versus.feign.failure", 
            "service", serviceName,
            "method", method,
            "exception", e.getClass().getSimpleName()
        ).increment();
        
        log.warn("Feign Client 호출 실패: service={}, method={}, error={}", 
            serviceName, method, e.getMessage());
    }

    /**
     * 보상 지급 실패 기록
     */
    public void recordRewardFailure(String roomId, String userId, Exception e) {
        String key = roomId + ":" + userId;
        rewardFailureCounts.merge(key, 1L, Long::sum);
        
        meterRegistry.counter("versus.reward.failure",
            "roomId", roomId,
            "userId", userId,
            "exception", e.getClass().getSimpleName()
        ).increment();
        
        log.error("보상 지급 실패: roomId={}, userId={}, error={}", 
            roomId, userId, e.getMessage());
    }

    /**
     * 응답 시간 기록
     */
    public Timer.Sample startTimer(String operation) {
        return Timer.start(meterRegistry);
    }

    public void recordTimer(Timer.Sample sample, String operation, String... tags) {
        if (tags.length % 2 != 0) {
            log.warn("Invalid tags length for operation {}: must be even (key-value pairs)", operation);
            sample.stop(meterRegistry.timer("versus.operation.duration", "operation", operation));
            return;
        }
        io.micrometer.core.instrument.Timer.Builder timerBuilder = 
            io.micrometer.core.instrument.Timer.builder("versus.operation.duration")
                .tag("operation", operation);
        for (int i = 0; i < tags.length; i += 2) {
            if (i + 1 < tags.length) {
                timerBuilder.tag(tags[i], tags[i + 1]);
            }
        }
        sample.stop(timerBuilder.register(meterRegistry));
    }

    /**
     * 주기적으로 실패 통계 로깅 (5분마다)
     */
    @Scheduled(fixedRate = 300000) // 5분
    public void logFailureStatistics() {
        if (!feignFailureCounts.isEmpty()) {
            log.warn("Feign Client 실패 통계 (최근 5분): {}", feignFailureCounts);
            feignFailureCounts.clear();
        }
        
        if (!rewardFailureCounts.isEmpty()) {
            log.warn("보상 지급 실패 통계 (최근 5분): {}", rewardFailureCounts);
            rewardFailureCounts.clear();
        }
    }
}


