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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Home", description = "홈 대시보드 전용 API")
@RestController
@RequestMapping("/api/progress/home")
@RequiredArgsConstructor
public class HomeController {

  private final HomeDashboardService homeDashboardService;

  @Operation(summary = "사용자 홈 개요 카드")
  @GetMapping("/overview")
  public ResponseEntity<HomeOverview> overview() {
    String userId = getCurrentUserId();
    return ResponseEntity.ok(homeDashboardService.overview(userId));
  }

  @Operation(summary = "학습 진행률 카드 데이터")
  @GetMapping("/progress-card")
  public ResponseEntity<HomeProgressCard> progressCard() {
    String userId = getCurrentUserId();
    return ResponseEntity.ok(homeDashboardService.progressCard(userId));
  }

  @Operation(summary = "실시간 랭킹 카드 데이터")
  @GetMapping("/ranking")
  public ResponseEntity<HomeRanking> ranking() {
    String userId = getCurrentUserId();
    return ResponseEntity.ok(homeDashboardService.ranking(userId));
  }

  @Operation(summary = "오늘의 성과/오늘 대비 변화")
  @GetMapping("/quick-stats")
  public ResponseEntity<HomeQuickStats> quickStats() {
    String userId = getCurrentUserId();
    return ResponseEntity.ok(homeDashboardService.quickStats(userId));
  }

  @Operation(summary = "홈 빠른 시작 메뉴")
  @GetMapping("/quick-menu")
  public ResponseEntity<HomeQuickMenu> quickMenu() {
    // 사용자별 개인화가 필요하다면 여기서 userId를 넘기도록 바꾸면 됩니다.
    // String userId = getCurrentUserId();
    return ResponseEntity.ok(homeDashboardService.quickMenu());
  }
}
