package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.ArraySchema;
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
      @Schema(description = "지난주 대비 정답률 변화(%)", example = "5.0")
      double weekAccuracyDelta,
      @Schema(description = "총 학습 시간(분)", example = "1440")
      long totalStudyMinutes,
      @Schema(description = "이번 주 학습 시간(분)", example = "320")
      long totalStudyMinutesThisWeek,
      @Schema(description = "연속 학습 일수", example = "7")
      int streakDays,
      @Schema(description = "지난 주(직전 7일) 문제 수", example = "210")
      long problemsLastWeek,
      @Schema(description = "총 정답 개수", example = "360")
      long totalCorrect,
      @Schema(description = "이번 주 평균 정답률(%)", example = "82.0")
      double weekAccuracy,
      @Schema(description = "지난 주 평균 정답률(%)", example = "77.0")
      double prevWeekAccuracy,
      @Schema(description = "지난 주 학습 시간(분)", example = "290")
      long totalStudyMinutesLastWeek
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

  @Schema(name = "TagAbilityResp")
  public record TagAbilityResp(
      @ArraySchema(schema = @Schema(implementation = TagAbility.class),
          arraySchema = @Schema(description = "태그별 능력치 목록"))
      List<TagAbility> items,
      @Schema(description = "약점 태그 목록(정답률이 낮은 순)", example = "[\"#데이터베이스\", \"#운영체제\"]")
      List<String> weaknessTags,
      @Schema(description = "약점 요약 메시지", example = "다음 태그 정답률이 낮습니다: #데이터베이스, #운영체제")
      String message
  ) {}

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
      @ArraySchema(schema = @Schema(implementation = RecentRecord.class),
          arraySchema = @Schema(description = "최근 학습 결과 목록(최신순)"))
      List<RecentRecord> records
  ) {}
}
