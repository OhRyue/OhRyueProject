package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;

public class XpDtos {

    @Schema(name = "XpEarnRequest", description = "XP 지급 요청")
    public record XpEarnRequest(
            @Schema(description = "활동 타입", example = "WRITTEN_MICRO", 
                    allowableValues = {"WRITTEN_MICRO", "PRACTICAL_MICRO", "WRITTEN_REVIEW", "PRACTICAL_REVIEW",
                                      "ASSIST_CORRECT", "ASSIST_WRONG", "DUEL_WIN", "DUEL_JOIN",
                                      "TOURNAMENT_WIN", "TOURNAMENT_JOIN", "GOLDENBELL_WIN", "GOLDENBELL_JOIN"})
            String activityType,
            @Schema(description = "세션 ID (중복 XP 지급 방지용)", example = "123")
            Long sessionId,
            @Schema(description = "토픽 ID (선택)", example = "11101")
            Long topicId,
            @Schema(description = "정답률 (0.0 ~ 100.0, 선택, 정답률 기반 XP 계산용)", example = "90.0")
            Double scorePct
    ) {}

    @Schema(name = "XpEarnResponse", description = "XP 지급 응답")
    public record XpEarnResponse(
            @Schema(description = "이번에 획득한 XP", example = "150")
            int earnedXp,
            @Schema(description = "지급 후 총 XP", example = "1200")
            long totalXp,
            @Schema(description = "현재 레벨", example = "3")
            int level,
            @Schema(description = "다음 레벨까지 필요한 XP", example = "450")
            int xpToNextLevel,
            @Schema(description = "이번 지급으로 인해 레벨업 여부", example = "false")
            boolean leveledUp,
            @Schema(description = "레벨업 보상 포인트 (없으면 0)", example = "0")
            int levelUpRewardPoints
    ) {}

    @Schema(name = "XpWalletResponse", description = "XP 지갑 조회 응답")
    public record XpWalletResponse(
            @Schema(description = "현재 총 XP", example = "1200")
            long xpTotal,
            @Schema(description = "현재 레벨", example = "3")
            int level,
            @Schema(description = "다음 레벨까지 필요한 XP", example = "450")
            int xpToNextLevel
    ) {}
}

