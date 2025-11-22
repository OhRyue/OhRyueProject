package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.SidebarDtos.SidebarSummary;
import com.OhRyue.certpilot.progress.service.SidebarService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static com.OhRyue.common.auth.AuthUserUtil.getCurrentUserId;

@Tag(name = "Progress - Sidebar", description = "좌측 레이아웃 사용자/상점 요약")
@RestController
@RequestMapping("/api/progress/sidebar")
@RequiredArgsConstructor
public class SidebarController {

  private final SidebarService sidebarService;

  @Operation(summary = "좌측 메뉴 사용자 카드/상점 요약")
  @GetMapping
  public ResponseEntity<SidebarSummary> sidebar() {
    String userId = getCurrentUserId();
    return ResponseEntity.ok(sidebarService.sidebar(userId));
  }
}
