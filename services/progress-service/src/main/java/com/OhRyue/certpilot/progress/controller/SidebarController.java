package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.SidebarDtos.SidebarSummary;
import com.OhRyue.certpilot.progress.service.SidebarService;
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

@Tag(name = "Progress - Sidebar", description = "좌측 레이아웃 사용자/상점 요약")
@RestController
@RequestMapping("/api/progress/sidebar")
@RequiredArgsConstructor
public class SidebarController {

  private final SidebarService sidebarService;

  @Operation(summary = "좌측 메뉴 사용자 카드/상점 요약")
  @GetMapping
  public ResponseEntity<SidebarSummary> sidebar(@RequestParam String userId,
                                                @RequestHeader(value = "Authorization", required = false) String authorization) {
    if (!StringUtils.hasText(userId) || !StringUtils.hasText(authorization)) {
      return ResponseEntity.status(401).build();
    }
    return ResponseEntity.ok(sidebarService.sidebar(authorization, userId));
  }
}

