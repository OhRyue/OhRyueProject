package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportPlusDtos.*;
import com.OhRyue.certpilot.study.service.ReportPlusService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Report+(태그 능력/시계열)")
@RestController
@RequestMapping("/api/study/report-plus")
@RequiredArgsConstructor
public class ReportPlusController {

  private final ReportPlusService reportPlus;

  @Operation(summary = "태그별 능력지수")
  @GetMapping("/tag-ability")
  public TagAbilityResp tagAbility(@RequestParam String userId,
                                   @RequestParam(required = false) String mode) {
    return reportPlus.tagAbility(userId, mode);
  }

  @Operation(summary = "최근 시계열(주/월)")
  @GetMapping("/timeseries")
  public TimeSeriesResp timeseries(@RequestParam String userId,
                                   @RequestParam(defaultValue = "WEEK") String range) {
    return reportPlus.timeseries(userId, range);
  }
}
