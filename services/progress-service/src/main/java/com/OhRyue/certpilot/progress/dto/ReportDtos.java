package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDate;
import java.util.List;

public class ReportDtos {

    @Schema(name = "Overview", description = "리포트 개요")
    public record Overview(
            @Schema(description = "총 풀었던 문제 수", example = "478")
            long totalProblems,
            @Schema(description = "이번 주에 푼 문제 수(최근 7일)", example = "245")
            long problemsThisWeek,
            @Schema(description = "전체 평균 정답률(%)", example = "78.0")
            double avgAccuracy,
            @Schema(description = "지난주 대비 정답률 변화(%)", example = "+5.0")
            double weekAccuracyDelta,
            @Schema(description = "총 학습 시간(분)", example = "1440")
            long totalStudyMinutes,
            @Schema(description = "이번 주 학습 시간(분)", example = "320")
            long totalStudyMinutesThisWeek,
            @Schema(description = "연속 학습 일수", example = "7")
            int streakDays
    ) {}

    @Schema(name = "TagAbility", description = "태그별 능력치")
    public record TagAbility(
            @Schema(description = "태그명", example = "데이터베이스")
            String tag,
            @Schema(description = "정답 개수", example = "38")
            long correct,
            @Schema(description = "전체 개수", example = "45")
            long total,
            @Schema(description = "정답률(%)", example = "84.4")
            double accuracy
    ) {}

    @Schema(name = "RecentItem", description = "일자별 최근 결과")
    public record RecentItem(
            @Schema(description = "일자(로컬, KST)", example = "2025-10-22")
            LocalDate date,
            @Schema(description = "모드(WRITTEN/PRACTICAL)", example = "WRITTEN")
            String mode,
            @Schema(description = "해당 일자 푼 문제 수", example = "20")
            long solved,
            @Schema(description = "해당 일자 정답 개수", example = "16")
            long correct,
            @Schema(description = "해당 일자 정답률(%)", example = "80.0")
            double accuracy
    ) {}

    @Schema(name = "TagAbilityResp")
    public record TagAbilityResp(
            @Schema(description = "태그별 능력치 목록")
            List<TagAbility> items
    ) {}

    @Schema(name = "RecentResp")
    public record RecentResp(
            @Schema(description = "최근 일자별 결과 목록(오름차순)")
            List<RecentItem> items
    ) {}
}
