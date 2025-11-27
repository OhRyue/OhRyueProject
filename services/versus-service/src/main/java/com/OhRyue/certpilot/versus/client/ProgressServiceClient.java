package com.OhRyue.certpilot.versus.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@FeignClient(
    name = "progress-service", 
    path = "/api/progress",
    fallback = ProgressServiceClientFallback.class
)
public interface ProgressServiceClient {

    /**
     * Versus 매치 결과 기록 및 보상 지급
     */
    @PostMapping("/versus/result")
    void recordVersusResult(@RequestBody VersusResultRequest request);

    record VersusResultRequest(
        String mode,  // DUEL, TOURNAMENT, GOLDENBELL
        Long roomId,
        String winner,
        List<ParticipantResult> participants,
        Integer questionCount,
        Long durationMs
    ) {}

    record ParticipantResult(
        String userId,
        Integer score,
        Integer rank,
        Integer correctCount,
        Integer totalCount,
        Long totalTimeMs
    ) {}
}

