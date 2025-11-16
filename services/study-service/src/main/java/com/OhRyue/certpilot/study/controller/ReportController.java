package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportDtos.*;
import com.OhRyue.certpilot.study.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

@Tag(name = "Report(요약/최근결과)")
@RestController
@RequestMapping("/api/study/report")
@RequiredArgsConstructor
public class ReportController {

  private final ReportService report;

  private String currentUserId() {
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    return authentication.getName();
  }

  @Operation(summary = "리포트 요약")
  @GetMapping("/summary")
  public ReportSummaryResp summary() {
    String userId = currentUserId();
    return report.summary(userId);
  }

  @Operation(summary = "학습 진행 카드 요약")
  @GetMapping("/progress-card")
  public ProgressCardResp progressCard(@RequestParam Long certId) {
    String userId = currentUserId();
    return report.progressCard(userId, certId);
  }

  @Operation(summary = "최근 학습 결과(일자별)")
  @GetMapping("/recent-daily")
  public RecentResultsResp recent(@RequestParam(defaultValue = "14") int days) {
    String userId = currentUserId();
    return report.recentDaily(userId, days);
  }
}
