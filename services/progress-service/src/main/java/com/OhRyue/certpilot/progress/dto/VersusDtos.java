package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.util.List;

public class VersusDtos {

    @Schema(description = "Versus 매치 결과 요청")
    public record VersusResultRequest(
        @NotBlank
        @Schema(description = "대전 모드 (DUEL, TOURNAMENT, GOLDENBELL)", 
            example = "DUEL", required = true)
        String mode,
        
        @NotNull
        @Schema(description = "방 ID", 
            example = "123", required = true)
        Long roomId,
        
        @Schema(description = "우승자 userId (1등 참가자)", 
            example = "user1")
        String winner,
        
        @Valid
        @Schema(description = "참가자별 결과 (점수 순으로 정렬)", 
            required = true)
        List<ParticipantResult> participants,
        
        @Min(1)
        @Schema(description = "총 문제 개수", 
            example = "10", required = true)
        Integer questionCount,
        
        @Min(0)
        @Schema(description = "매치 총 소요 시간 (밀리초)", 
            example = "120000", required = true)
        Long durationMs
    ) {}

    @Schema(description = "참가자 결과")
    public record ParticipantResult(
        @NotBlank
        @Schema(description = "사용자 ID", 
            example = "user1", required = true)
        String userId,
        
        @Min(0)
        @Schema(description = "최종 점수", 
            example = "8500", required = true)
        Integer score,
        
        @Schema(description = "순위 (1등이 1)", 
            example = "1")
        Integer rank,
        
        @Min(0)
        @Schema(description = "정답 개수", 
            example = "8", required = true)
        Integer correctCount,
        
        @Min(0)
        @Schema(description = "총 문제 개수", 
            example = "10", required = true)
        Integer totalCount,
        
        @Min(0)
        @Schema(description = "총 소요 시간 (밀리초)", 
            example = "45000", required = true)
        Long totalTimeMs
    ) {}
}

