package com.OhRyue.certpilot.community.dto;

import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;
import java.util.List;

public class ModerationDtos {

  @Schema(description = "신고 요청")
  public record ReportRequest(
      @NotNull ReactionTargetType targetType,
      @NotNull Long targetId,
      @NotBlank String reason
  ) {}

  @Schema(description = "신고 결과")
  public record ReportResponse(
      Long reportId,
      ReactionTargetType targetType,
      Long targetId,
      String status,
      Instant createdAt
  ) {}

  @Schema(description = "차단 요청")
  public record BlockRequest(
      @NotBlank String blockedUserId
  ) {}

  @Schema(description = "차단 목록 응답")
  public record BlockListResponse(
      List<BlockEntry> items
  ) {}

  @Schema(description = "차단 대상")
  public record BlockEntry(
      String blockedUserId,
      Instant createdAt
  ) {}
}

