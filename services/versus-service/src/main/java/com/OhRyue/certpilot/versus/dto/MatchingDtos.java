package com.OhRyue.certpilot.versus.dto;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;

/**
 * 실시간 매칭 관련 DTO
 */
public class MatchingDtos {

    @Schema(description = "매칭 요청")
    public record MatchRequest(
        @NotNull
        @Schema(description = "대전 모드 (DUEL: 1:1 배틀, TOURNAMENT: 토너먼트)", 
            example = "DUEL", required = true)
        MatchMode mode,
        
        @NotBlank
        @Schema(description = "자격증 ID", example = "1", required = true)
        String certId,
        
        @Schema(description = "매칭 모드 (CATEGORY: 카테고리 모드, DIFFICULTY: 난이도 모드)", 
            example = "CATEGORY", required = true)
        String matchingMode,  // CATEGORY, DIFFICULTY
        
        @Schema(description = "카테고리 모드일 때: 2레벨 토픽 ID (1개만 선택)", 
            example = "101")
        Long topicId,  // 카테고리 모드일 때 필수
        
        @Schema(description = "난이도 모드일 때: 난이도 (EASY, NORMAL, HARD)", 
            example = "NORMAL")
        String difficulty,  // 난이도 모드일 때 필수
        
        @Schema(description = "시험 모드 (WRITTEN, PRACTICAL)", 
            example = "WRITTEN", required = true)
        String examMode
    ) {}

    @Schema(description = "매칭 상태 조회 응답")
    public record MatchStatusResp(
        @Schema(description = "매칭 중 여부", example = "true")
        boolean matching,
        
        @Schema(description = "매칭된 방 ID (매칭 성공 시)", example = "1101")
        Long roomId,
        
        @Schema(description = "현재 매칭 풀의 대기 인원 수", example = "1")
        int waitingCount,
        
        @Schema(description = "매칭 시작 시간", example = "2024-12-25T14:00:00Z")
        Instant startedAt
    ) {}

    @Schema(description = "매칭 취소 요청")
    public record CancelMatchRequest(
        @NotNull
        @Schema(description = "대전 모드", example = "DUEL", required = true)
        MatchMode mode
    ) {}
}


