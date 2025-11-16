package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.ReportPlusDtos.*;
import com.OhRyue.certpilot.study.service.ReportPlusService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

@Tag(name = "Report+(태그 능력/시계열)")
@RestController
@RequestMapping("/api/study/report-plus")
@RequiredArgsConstructor
public class ReportPlusController {

  private final ReportPlusService reportPlus;

  private String currentUserId() {
    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
    return authentication.getName();
  }

  @Operation(summary = "태그별 능력지수")
  @GetMapping("/tag-ability")
  public TagAbilityResp tagAbility(@RequestParam(required = false) String mode) {
    String userId = currentUserId();
    return reportPlus.tagAbility(userId, mode);
  }

  @Operation(summary = "최근 시계열(주/월)")
  @GetMapping("/timeseries")
  public TimeSeriesResp timeseries(@RequestParam(defaultValue = "WEEK") String range) {
    String userId = currentUserId();
    return reportPlus.timeseries(userId, range);
  }
}
