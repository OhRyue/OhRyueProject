package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.AssistGoalDaily;
import com.OhRyue.certpilot.progress.dto.GoalDtos;
import com.OhRyue.certpilot.progress.service.GoalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Progress - Goals", description = "보조학습 목표 관리 APIs")
@RestController
@RequestMapping("/api/progress/goal")
@RequiredArgsConstructor
public class GoalController {

  private final GoalService goals;

  @Operation(summary = "오늘 목표 조회")
  @GetMapping("/today")
  public AssistGoalDaily getToday(@RequestParam String userId) {
    return goals.getToday(userId);
  }

  @Operation(summary = "오늘 목표 설정")
  @PostMapping("/today/target")
  public AssistGoalDaily setTarget(@RequestParam String userId, @RequestParam int target) {
    return goals.setTarget(userId, target);
  }

  @Operation(summary = "오늘 목표 진행 증가")
  @PostMapping("/today/increment")
  public AssistGoalDaily increment(@RequestParam String userId,
                                   @RequestParam(defaultValue = "1") int inc) {
    return goals.increment(userId, inc);
  }

  @Operation(summary = "목표/주간 통계 요약")
  @GetMapping("/summary")
  public GoalDtos.AssistSummary summary(@RequestParam String userId) {
    return goals.summary(userId);
  }
}
