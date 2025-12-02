package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.Instant;
import java.time.LocalDate;

@FeignClient(
    name = "progress-query-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/progress"
)
public interface ProgressQueryClient {

  /**
   * 오늘 보조학습 목표 상태 조회
   * - Progress: GET /api/progress/goal/today
   * - JWT 기반으로 userId는 서버에서 AuthUserUtil로 추출
   */
  @GetMapping("/goal/today")
  GoalToday getTodayGoal();

  /**
   * 리포트 개요 조회
   * - Progress: GET /api/progress/report/overview
   * - mode: "WRITTEN" / "PRACTICAL"
   * - userId는 JWT에서 추출
   */
  @GetMapping("/report/overview")
  Overview overview(
      @RequestParam(value = "mode", defaultValue = "WRITTEN") String mode
  );

  /**
   * 태그별 능력지수 조회
   * - Progress: GET /api/progress/report/ability-by-tag
   * - mode: "WRITTEN" / "PRACTICAL"
   * - userId는 JWT에서 추출
   */
  @GetMapping("/report/ability-by-tag")
  TagAbilityResp abilityByTag(
      @RequestParam(value = "mode", defaultValue = "WRITTEN") String mode,
      @RequestParam(value = "limit", defaultValue = "20") int limit
  );

  // === DTOs: Progress 쪽 GoalDtos.DailyGoalStatus, ReportDtos.Overview 에 맞춰 작성 ===

  // /goal/today 응답
  record GoalToday(
      LocalDate date,
      int target,
      int progress,
      int remaining,
      boolean completed,
      Instant updatedAt
  ) {}

  // /report/overview 응답
  record Overview(
      long totalProblems,
      long problemsThisWeek,
      double avgAccuracy,
      double weekAccuracyDelta,
      long totalStudyMinutes,
      long totalStudyMinutesThisWeek,
      int streakDays,
      long problemsLastWeek,
      long totalCorrect,
      double weekAccuracy,
      double prevWeekAccuracy,
      long totalStudyMinutesLastWeek
  ) {}

  // /report/ability-by-tag 응답
  record TagAbilityResp(
      java.util.List<TagAbility> items,
      java.util.List<String> weaknessTags,
      String message
  ) {
    record TagAbility(
        String tag,
        int correct,
        int total,
        double accuracy
    ) {}
  }
}
