package com.OhRyue.certpilot.versus.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@FeignClient(
    name = "progress-service", 
    path = "/api/progress",
    url = "${services.progress.url:http://progress-service:8083}",  // Docker 환경에서는 직접 URL 사용, Eureka 실패 시 대비
    fallback = ProgressServiceClientFallback.class
)
public interface ProgressServiceClient {

    /**
     * Versus 매치 결과 기록 및 보상 지급
     */
    @PostMapping("/versus/result")
    VersusResultResponse recordVersusResult(@RequestBody VersusResultRequest request);

    record VersusResultRequest(
        String mode,  // DUEL, TOURNAMENT, GOLDENBELL
        Long roomId,
        String winner,
        List<ParticipantResult> participants,
        Integer questionCount,
        Long durationMs,
        String examMode,  // WRITTEN, PRACTICAL
        Boolean botMatch
    ) {}

    record ParticipantResult(
        String userId,
        Integer score,
        Integer rank,
        Integer correctCount,
        Integer totalCount,
        Long totalTimeMs,
        List<AnswerDetail> answers  // 개별 답안 목록
    ) {}
    
    record AnswerDetail(
        Long questionId,
        String userAnswer,
        Boolean isCorrect,
        Integer timeMs,
        Integer scoreDelta,
        Integer roundNo,
        String phase
    ) {}

    record VersusResultResponse(
        String mode,
        Long roomId,
        List<XpResult> xpResults
    ) {}

    record XpResult(
        String userId,
        Integer xpDelta,
        String reason,
        Long totalXp,
        Boolean leveledUp
    ) {}
}

