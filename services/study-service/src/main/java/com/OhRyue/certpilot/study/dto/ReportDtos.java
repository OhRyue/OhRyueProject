package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.LocalDate;
import java.util.List;

public class ReportDtos {

  @Schema(description = "리포트 요약 응답")
  public record ReportSummaryResp(
      @Schema(description = "총 푼 문제 수") long totalSolved,
      @Schema(description = "최근 7일 푼 문제 수") long weeklySolved,
      @Schema(description = "전체 평균 정답률(%)") double avgAccuracy,
      @Schema(description = "최근 7일 평균 정답률(%)") double last7dAccuracy,
      @Schema(description = "직전 7일 평균 정답률(%)") double prev7dAccuracy,
      @Schema(description = "최근 7일 - 직전 7일 정답률 증감(%)") double deltaAccuracy,
      @Schema(description = "연속 학습 일수(일)") int streakDays
  ) {}

  @Schema(description = "최근 학습 결과 - 일자별 집계 한 줄")
  public record RecentDailyItem(
      @Schema(description = "일자") LocalDate date,
      @Schema(description = "맞힌 개수") int correct,
      @Schema(description = "푼 개수") int total,
      @Schema(description = "정답률(%)") double accuracy
  ) {}

  @Schema(description = "최근 학습 결과 목록")
  public record RecentResultsResp(
      @Schema(description = "일자별 결과(최신순)") List<RecentDailyItem> items
  ) {}
}
