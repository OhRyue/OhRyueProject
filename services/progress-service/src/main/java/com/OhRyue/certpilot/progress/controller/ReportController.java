package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.ReportDtos.*;
import com.OhRyue.certpilot.progress.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/progress/report")
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

  @Operation(
      summary = "최근 학습 기록(최신순)",
      description = "유저의 최근 학습 결과를 최신순으로 반환합니다. (날짜/유형/파트이름/정답수/전체/정답률)",
      responses = {
          @ApiResponse(
              responseCode = "200",
              description = "성공",
              content = @Content(
                  schema = @Schema(implementation = RecentRecordsResp.class),
                  examples = @ExampleObject(
                      name = "RecentRecordsResp 예시",
                      value = """
                      {
                        "records": [
                          { "date": "2025-11-06", "type": "Micro",  "partTitle": "데이터베이스 기초", "total": 20, "correct": 10, "accuracy": 50.0 },
                          { "date": "2025-11-05", "type": "Review", "partTitle": "정규화",         "total": 20, "correct": 18, "accuracy": 90.0 },
                          { "date": "2025-11-04", "type": "Assist", "partTitle": "네트워크 기초", "total": 10, "correct": 7,  "accuracy": 70.0 }
                        ]
                      }
                      """
                  )
              )
          )
      }
  )
  @GetMapping("/recent-records")
  public RecentRecordsResp recentRecords(
      @RequestParam String userId,
      @RequestParam(defaultValue = "30") int limit
  ) {
    return report.recentRecords(userId, limit);
  }
}
