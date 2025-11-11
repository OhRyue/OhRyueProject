package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import com.OhRyue.certpilot.progress.service.StreakService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/progress/streak")
@RequiredArgsConstructor
public class StreakController {
  private final StreakService streak;

  @Operation(summary="오늘 streak 반영")
  @PostMapping("/tick")
  public UserStreak tick(@RequestParam String userId){
    return streak.tickToday(userId);
  }

  @Operation(summary="streak 조회")
  @GetMapping
  public UserStreak get(@RequestParam String userId){
    return streak.get(userId);
  }
}
