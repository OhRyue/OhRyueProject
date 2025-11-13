package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeOverview;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeProgressCard;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeQuickMenu;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeQuickStats;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeRanking;
import com.OhRyue.certpilot.progress.service.HomeDashboardService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "Progress - Home", description = "홈 대시보드 전용 API")
@RestController
@RequestMapping("/api/progress/home")
@RequiredArgsConstructor
public class HomeController {

  private final HomeDashboardService homeDashboardService;

  @Operation(summary = "사용자 홈 개요 카드")
  @GetMapping("/overview")
  public ResponseEntity<HomeOverview> overview(@RequestParam String userId,
                                               @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!isAuthorized(userId, authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(homeDashboardService.overview(authorization, userId));
  }

  @Operation(summary = "학습 진행률 카드 데이터")
  @GetMapping("/progress-card")
  public ResponseEntity<HomeProgressCard> progressCard(@RequestParam String userId,
                                                       @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!isAuthorized(userId, authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(homeDashboardService.progressCard(authorization, userId));
  }

  @Operation(summary = "실시간 랭킹 카드 데이터")
  @GetMapping("/ranking")
  public ResponseEntity<HomeRanking> ranking(@RequestParam String userId,
                                             @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!isAuthorized(userId, authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(homeDashboardService.ranking(authorization, userId));
  }

  @Operation(summary = "오늘의 성과/오늘 대비 변화")
  @GetMapping("/quick-stats")
  public ResponseEntity<HomeQuickStats> quickStats(@RequestParam String userId,
                                                   @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!isAuthorized(userId, authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(homeDashboardService.quickStats(userId));
  }

  @Operation(summary = "홈 빠른 시작 메뉴")
  @GetMapping("/quick-menu")
  public ResponseEntity<HomeQuickMenu> quickMenu(@RequestParam String userId,
                                                 @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!isAuthorized(userId, authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(homeDashboardService.quickMenu());
  }

  private boolean isAuthorized(String userId, String authorization) {
    return StringUtils.hasText(userId) && StringUtils.hasText(authorization);
  }
}

