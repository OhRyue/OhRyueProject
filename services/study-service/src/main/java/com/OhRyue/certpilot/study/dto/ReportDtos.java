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

  @Schema(description = "학습 진행 카드 요약")
  public record ProgressCardResp(
      @Schema(description = "총 토픽 수 (micro + review)") int totalTopics,
      @Schema(description = "완료된 토픽 수 (truly_completed만)") int completedTopics,
      @Schema(description = "미완료 토픽 수") int pendingTopics,
      @Schema(description = "달성률(%)") double completionRate,
      @Schema(description = "마지막 학습 시각(KST 기준 ISO8601)") String lastStudiedAt
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

  // 스펙 v1.0: 최근 학습 결과 (세션 기반)
  @Schema(name = "RecentRecord", description = "최근 학습 결과(최신순 카드 항목)")
  public record RecentRecord(
      @Schema(description = "날짜(로컬, KST)", example = "2025-10-22")
      LocalDate date,
      @Schema(description = "문제 유형(Micro/Review/Assist 등)", example = "Micro")
      String type,
      @Schema(description = "파트/토픽 이름", example = "DB기초")
      String partTitle,
      @Schema(description = "전체 문항 수", example = "20")
      int total,
      @Schema(description = "정답 수", example = "10")
      int correct,
      @Schema(description = "정답률(%)", example = "50.0")
      double accuracy
  ) {}

  @Schema(name = "RecentRecordsResp", description = "최근 학습 결과 응답(최신순)")
  public record RecentRecordsResp(
      @Schema(description = "최근 학습 결과 목록(최신순)")
      List<RecentRecord> records
  ) {}
}
