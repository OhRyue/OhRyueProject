package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.rank.RankDtos;
import com.OhRyue.certpilot.progress.service.BadgeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Badge", description = "배지 시스템 APIs")
@RestController
@RequestMapping("/api/progress/badge")
@RequiredArgsConstructor
public class BadgeController {

  private final BadgeService badgeService;

  @Operation(summary = "내 배지 조회", description = "획득한 배지 목록과 전체 배지 카탈로그를 반환합니다.")
  @GetMapping("/my")
  public RankDtos.BadgeStatusResponse getMyBadges() {
    String userId = getCurrentUserId();
    return badgeService.status(userId);
  }

  @Operation(summary = "배지 평가 및 지급", description = "현재 사용자의 배지 조건을 평가하고 새로운 배지를 지급합니다.")
  @PostMapping("/evaluate")
  public RankDtos.BadgeStatusResponse evaluateBadges() {
    String userId = getCurrentUserId();
    return badgeService.evaluate(userId);
  }
}















