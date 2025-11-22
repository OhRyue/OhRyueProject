package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import com.OhRyue.certpilot.progress.service.StreakService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Streak", description = "연속 학습 관리 APIs")
@RestController
@RequestMapping("/api/progress/streak")
@RequiredArgsConstructor
public class StreakController {

  private final StreakService streak;

  @Operation(summary = "오늘 streak 반영")
  @PostMapping("/tick")
  public UserStreak tick() {
    String userId = getCurrentUserId();
    return streak.tickToday(userId);
  }

  @Operation(summary = "streak 조회")
  @GetMapping
  public UserStreak get() {
    String userId = getCurrentUserId();
    return streak.get(userId);
  }
}
