package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.Instant;
import java.util.List;

public class StoreDtos {

  private StoreDtos() {}

  @Schema(description = "사용자 포인트 및 인벤토리 현황")
  public record StoreUserSummary(
      @Schema(description = "사용자 ID") String userId,
      @Schema(description = "현재 포인트") long pointBalance,
      @Schema(description = "보유 아이템 수") int ownedItemCount
  ) {}

  @Schema(description = "상점 아이템 요약")
  public record StoreItemView(
      @Schema(description = "아이템 ID") Long itemId,
      @Schema(description = "아이템명") String name,
      @Schema(description = "설명") String description,
      @Schema(description = "가격") int price,
      @Schema(description = "보유 여부") boolean owned,
      @Schema(description = "구매 제한 수량") Integer limitPerUser,
      @Schema(description = "활성 여부") boolean active
  ) {}

  @Schema(description = "상점 카탈로그 응답")
  public record StoreCatalog(
      StoreUserSummary user,
      List<StoreItemView> items,
      @Schema(description = "카탈로그 생성 시각") Instant generatedAt
  ) {}
}

