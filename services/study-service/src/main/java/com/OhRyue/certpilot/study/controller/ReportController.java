package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportDtos.*;
import com.OhRyue.certpilot.study.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Report(요약/최근결과)")
@RestController
@RequestMapping("/api/study/report")
@RequiredArgsConstructor
public class ReportController {

  private final ReportService report;

  @Operation(summary = "리포트 요약")
  @GetMapping("/summary")
  public ReportSummaryResp summary(@RequestParam String userId) {
    return report.summary(userId);
  }

  @Operation(summary = "학습 진행 카드 요약")
  @GetMapping("/progress-card")
  public ProgressCardResp progressCard(@RequestParam String userId,
                                       @RequestParam Long certId) {
    return report.progressCard(userId, certId);
  }

  @Operation(summary = "최근 학습 결과(일자별)")
  @GetMapping("/recent-daily")
  public RecentResultsResp recent(@RequestParam String userId,
                                  @RequestParam(defaultValue = "14") int days) {
    return report.recentDaily(userId, days);
  }
}
