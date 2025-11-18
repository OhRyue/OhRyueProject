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
  public ProgressCardResp progressCard(
      @RequestParam(required = false) String userId,
      @RequestParam Long certId) {
    // 서비스 간 호출 시 userId 파라미터 사용, 프론트엔드 호출 시 JWT에서 추출
    String finalUserId = userId != null ? userId : currentUserId();
    return report.progressCard(finalUserId, certId);
  }

  @Operation(summary = "최근 학습 결과(일자별)")
  @GetMapping("/recent-daily")
  public RecentResultsResp recent(@RequestParam(defaultValue = "14") int days) {
    String userId = currentUserId();
    return report.recentDaily(userId, days);
  }

  @Operation(summary = "최근 학습 결과(세션 기반) - 스펙 v1.0", 
             description = "세션별 학습 결과를 반환합니다. 날짜/유형/파트이름/정답수/전체/정답률")
  @GetMapping("/recent-records")
  public com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecordsResp recentRecords(
      @RequestParam(required = false) String userId,
      @RequestParam(defaultValue = "30") int limit) {
    // 서비스 간 호출 시 userId 파라미터 사용, 프론트엔드 호출 시 JWT에서 추출
    String finalUserId = userId != null ? userId : currentUserId();
    return report.recentRecords(finalUserId, limit);
  }
}
