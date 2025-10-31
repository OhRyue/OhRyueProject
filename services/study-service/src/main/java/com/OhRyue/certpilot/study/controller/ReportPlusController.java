package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportPlusDtos.TagAbilityResp;
import com.OhRyue.certpilot.study.dto.ReportPlusDtos.TimeSeriesResp;
import com.OhRyue.certpilot.study.service.ReportPlusService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/report")
@RequiredArgsConstructor
public class ReportPlusController {

  private final ReportPlusService service;

  @Operation(summary = "태그별 능력지수(필기/실기 모드 선택 가능)")
  @GetMapping("/tags")
  public TagAbilityResp tags(@RequestParam String userId,
                             @RequestParam(required = false) String mode) {
    return service.tagAbility(userId, mode);
  }

  @Operation(summary = "시계열(최근 7일 or 30일)")
  @GetMapping("/timeseries")
  public TimeSeriesResp timeseries(@RequestParam String userId,
                                   @RequestParam(defaultValue = "WEEK") String range) {
    return service.timeseries(userId, range);
  }
}
