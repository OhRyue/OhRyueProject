package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.ReportDtos.*;
import com.OhRyue.certpilot.progress.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/progress/report") // 게이트웨이 최신 규칙(/api/progress/**)에 맞춰 경로 정정
@RequiredArgsConstructor
public class ReportController {

  private final ReportService report;

  @Operation(summary = "리포트 개요", description = "총 학습시간/문항수/정답률, 주간 변화, 연속학습일 계산")
  @GetMapping("/overview")
  public Overview overview(
      @RequestParam String userId,
      @RequestParam(defaultValue = "WRITTEN") String mode
  ) {
    return report.overview(userId, mode);
  }

  @Operation(summary = "태그별 능력지수", description = "태그별 정답/전체/정답률 Top-N")
  @GetMapping("/ability-by-tag")
  public TagAbilityResp abilityByTag(
      @RequestParam String userId,
      @RequestParam(defaultValue = "WRITTEN") String mode,
      @RequestParam(defaultValue = "20") int limit
  ) {
    return report.abilityByTag(userId, mode, limit);
  }

  @Operation(summary = "최근 학습 결과(일별)", description = "최근 N일 동안 일자별 푼 문제/정답/정답률")
  @GetMapping("/recent-daily")
  public RecentResp recentDaily(
      @RequestParam String userId,
      @RequestParam(defaultValue = "WRITTEN") String mode,
      @RequestParam(defaultValue = "14") int days
  ) {
    return report.recentDaily(userId, mode, days);
  }
}
