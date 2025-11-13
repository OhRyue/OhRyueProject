package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.Instant;
import java.time.LocalDate;

public final class GoalDtos {

  private GoalDtos() {}

  @Schema(description = "하루 목표 상태")
  public record DailyGoalStatus(
      @Schema(description = "날짜") LocalDate date,
      @Schema(description = "목표 문제 수") int target,
      @Schema(description = "진행 문제 수") int progress,
      @Schema(description = "남은 문제 수") int remaining,
      @Schema(description = "목표 달성 여부") boolean completed,
      @Schema(description = "최근 갱신 시각") Instant updatedAt
  ) {}

  @Schema(description = "주간 학습 통계")
  public record WeeklyStats(
      @Schema(description = "ISO 주차 키 (yyyy-Www)") String weekIso,
      @Schema(description = "주간 풀이 문제 수") int solvedCount,
      @Schema(description = "주간 정답 수") int correctCount,
      @Schema(description = "주간 평균 정답률(%)") double accuracy,
      @Schema(description = "목표 달성 일수") int goalCompletedDays,
      @Schema(description = "집계 일수") int totalTrackedDays
  ) {}

  @Schema(description = "보조 학습 대시보드 요약")
  public record AssistSummary(
      DailyGoalStatus today,
      WeeklyStats week
  ) {}
}

