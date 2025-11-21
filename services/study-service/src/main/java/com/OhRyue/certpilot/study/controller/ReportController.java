package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportDtos.*;
import com.OhRyue.certpilot.study.service.ReportService;
import com.OhRyue.common.auth.AuthUserUtil;
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
    public ReportSummaryResp summary() {
        String userId = AuthUserUtil.getCurrentUserId();
        return report.summary(userId);
    }

    @Operation(summary = "학습 진행 카드 요약")
    @GetMapping("/progress-card")
    public ProgressCardResp progressCard(@RequestParam Long certId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return report.progressCard(userId, certId);
    }

    @Operation(summary = "최근 학습 결과(일자별)")
    @GetMapping("/recent-daily")
    public RecentResultsResp recent(@RequestParam(defaultValue = "14") int days) {
        String userId = AuthUserUtil.getCurrentUserId();
        return report.recentDaily(userId, days);
    }

    @Operation(summary = "최근 학습 결과(세션 기반) - 스펙 v1.0",
            description = "세션별 학습 결과를 반환합니다. 날짜/유형/파트이름/정답수/전체/정답률")
    @GetMapping("/recent-records")
    public com.OhRyue.certpilot.study.dto.ReportDtos.RecentRecordsResp recentRecords(
            @RequestParam(defaultValue = "30") int limit) {
        String userId = AuthUserUtil.getCurrentUserId();
        return report.recentRecords(userId, limit);
    }
}
