package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.LocalDate;
import java.util.List;

public class ReportPlusDtos {

  @Schema(description = "태그별 능력지수")
  public record TagAbilityItem(String tag, int correct, int total, double accuracy) {}

  public record TagAbilityResp(List<TagAbilityItem> items) {}

  @Schema(description = "시계열 포인트")
  public record TSPoint(LocalDate date, int correct, int total, double accuracy){}

  public record TimeSeriesResp(List<TSPoint> items) {}
}
