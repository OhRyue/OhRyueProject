package com.OhRyue.certpilot.progress.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public final class SidebarDtos {

  private SidebarDtos() {}

  @Schema(description = "좌측 메뉴 사용자 카드")
  public record SidebarUserCard(
      String userId,
      String nickname,
      Long skinId,
      int level,
      long xpTotal,
      int streakDays,
      long pointBalance
  ) {}

  @Schema(description = "빠른 이동 링크")
  public record QuickLink(String label, String path, String icon) {}

  @Schema(description = "상점 미리보기 정보")
  public record StorePreview(
      long pointBalance,
      int ownedItemCount,
      int activeItemCount
  ) {}

  @Schema(description = "좌측 사이드바 요약")
  public record SidebarSummary(
      SidebarUserCard user,
      StorePreview store,
      List<QuickLink> quickLinks
  ) {}
}

