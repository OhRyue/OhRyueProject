package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportDtos.RecentResultsResp;
import com.OhRyue.certpilot.study.dto.ReportDtos.ReportSummaryResp;
import com.OhRyue.certpilot.study.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/report")
@RequiredArgsConstructor
public class ReportController {

  private final ReportService reportService;

  @Operation(
      summary = "학습 리포트 요약",
      description = "총 푼 문제 수, 최근 7일 푼 문제 수, 평균 정답률, 최근 7일/직전 7일 정답률 및 증감, 연속 학습 일수 제공",
      responses = @ApiResponse(responseCode = "200", content = @Content(
          examples = @ExampleObject(value = """
                    {
                      "totalSolved": 478,
                      "weeklySolved": 245,
                      "avgAccuracy": 78.12,
                      "last7dAccuracy": 80.0,
                      "prev7dAccuracy": 75.0,
                      "deltaAccuracy": 5.0,
                      "streakDays": 7
                    }
                    """)
      ))
  )
  @GetMapping("/summary")
  public ReportSummaryResp summary(@RequestParam String userId) {
    return reportService.summary(userId);
  }

  @Operation(
      summary = "최근 학습 결과(일자별)",
      description = "최근 N일(기본 14일) 날짜별 정답/전체/정답률을 최신순으로 반환",
      responses = @ApiResponse(responseCode = "200", content = @Content(
          examples = @ExampleObject(value = """
                    {
                      "items": [
                        {"date":"2025-10-31","correct":10,"total":12,"accuracy":83.33},
                        {"date":"2025-10-30","correct":5,"total":10,"accuracy":50.0}
                      ]
                    }
                    """)
      ))
  )
  @GetMapping("/recent")
  public RecentResultsResp recent(@RequestParam String userId,
                                  @RequestParam(required = false, defaultValue = "14") int days) {
    return reportService.recentDaily(userId, days);
  }
}
