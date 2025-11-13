package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.time.LocalDate;

@FeignClient(
    name = "progress-query-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/progress"
)
public interface ProgressQueryClient {

  @GetMapping("/goal/today")
  GoalToday getTodayGoal(@RequestParam("userId") String userId);

  @GetMapping("/report/overview")
  Overview overview(@RequestParam("userId") String userId,
                    @RequestParam(value = "mode", defaultValue = "WRITTEN") String mode);

  record GoalToday(
      String userId,
      LocalDate date,
      Integer targetCount,
      Integer progressCount
  ) {}

  record Overview(
      long totalProblems,
      long problemsThisWeek,
      double avgAccuracy,
      double weekAccuracyDelta,
      long totalStudyMinutes,
      long totalStudyMinutesThisWeek,
      int streakDays
  ) {}
}

