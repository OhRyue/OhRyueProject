package com.OhRyue.certpilot.progress.dto.rank;

import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;
import java.util.List;

public class RankDtos {

  public record LeaderboardEntry(
      String userId,
      String nickname,
      long score,
      int rank,
      Long xp,
      Integer streak,
      Instant lastUpdatedAt
  ) {}

  public record LeaderboardResponse(
      RankScope scope,
      String reference,
      Instant generatedAt,
      List<LeaderboardEntry> top,
      LeaderboardEntry me,
      int page,
      int size,
      long totalElements,
      int totalPages
  ) {}

  public record LeaderboardHistoryResponse(
      RankScope scope,
      String reference,
      Instant generatedAt,
      List<LeaderboardEntry> entries
  ) {}

  public record BadgeSummary(
      String code,
      String name,
      String rarity,
      Instant earnedAt
  ) {}

  public record BadgeCatalogItem(
      String code,
      String name,
      String rarity,
      boolean owned
  ) {}

  public record BadgeStatusResponse(
      List<BadgeSummary> earned,
      List<BadgeCatalogItem> catalog,
      BadgeStats stats
  ) {}

  public record BadgeEvaluateRequest(
      @NotBlank String userId
  ) {}

  public record LeaderboardRecomputeRequest(
      @NotNull RankScope scope,
      @NotBlank String referenceDate
  ) {}

  public record BadgeStats(
      int totalEarned,
      int totalAvailable,
      List<BadgeRarityStat> byRarity
  ) {}

  public record BadgeRarityStat(
      String rarity,
      int earned,
      int total
  ) {}
}
