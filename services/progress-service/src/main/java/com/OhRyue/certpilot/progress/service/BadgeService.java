package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.*;
import com.OhRyue.certpilot.progress.domain.enums.Rarity;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos.BadgeRarityStat;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos.BadgeStats;
import com.OhRyue.certpilot.progress.repository.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class BadgeService {

  private static final List<BadgeSeed> DEFAULT_BADGES = List.of(
      new BadgeSeed("FIRST_SOLVE", "첫 문제 해결", Rarity.common, Map.of(
          "criteria", "user_answer_count>=1",
          "description", "첫 문제를 풀면 획득합니다."
      )),
      new BadgeSeed("STREAK_BRONZE", "7일 연속 학습", Rarity.rare, Map.of(
          "criteria", "streak_current>=7",
          "description", "연속 학습 7일을 달성하면 획득합니다."
      )),
      new BadgeSeed("XP_MASTER", "누적 XP 3000", Rarity.epic, Map.of(
          "criteria", "xp_total>=3000",
          "description", "누적 XP 3000을 돌파하면 획득합니다."
      )),
      new BadgeSeed("WEEKLY_WARRIOR", "주간 학습 매니아", Rarity.rare, Map.of(
          "criteria", "weekly_solved>=70",
          "description", "한 주 동안 70문제를 해결하면 획득합니다."
      )),
      new BadgeSeed("TAG_SPECIALIST", "태그 스페셜리스트", Rarity.rare, Map.of(
          "criteria", "tag_accuracy>=90,total>=20",
          "description", "특정 태그에서 정답률 90% · 누적 20문제를 달성하면 획득합니다."
      ))
  );

  private final BadgeCatalogRepository badgeCatalogRepository;
  private final UserBadgeRepository userBadgeRepository;
  private final UserAnswerRepository userAnswerRepository;
  private final UserStreakRepository userStreakRepository;
  private final UserXpWalletRepository userXpWalletRepository;
  private final ReportWeeklyRepository reportWeeklyRepository;
  private final ReportTagSkillRepository reportTagSkillRepository;
  private final ObjectMapper objectMapper;

  @PostConstruct
  @Transactional
  public void seedBadges() {
    for (BadgeSeed seed : DEFAULT_BADGES) {
      badgeCatalogRepository.findByCode(seed.code())
          .orElseGet(() -> badgeCatalogRepository.save(BadgeCatalog.builder()
              .code(seed.code())
              .name(seed.name())
              .rarity(seed.rarity())
              .ruleJson(writeJson(seed.rule()))
              .build()));
    }
  }

  @Transactional(readOnly = true)
  public RankDtos.BadgeStatusResponse status(String userId) {
    List<BadgeCatalog> catalog = badgeCatalogRepository.findAll();
    List<UserBadge> earned = userBadgeRepository.findByUserIdOrderByEarnedAtDesc(userId);
    Set<Long> earnedIds = earned.stream().map(UserBadge::getBadgeId).collect(Collectors.toSet());

    List<RankDtos.BadgeSummary> summaries = earned.stream()
        .map(b -> {
          BadgeCatalog meta = catalog.stream()
              .filter(c -> c.getId().equals(b.getBadgeId()))
              .findFirst()
              .orElse(null);
          if (meta == null) {
            return null;
          }
          return new RankDtos.BadgeSummary(
              meta.getCode(),
              meta.getName(),
              meta.getRarity().name(),
              b.getEarnedAt()
          );
        })
        .filter(Objects::nonNull)
        .toList();

    List<RankDtos.BadgeCatalogItem> catalogItems = catalog.stream()
        .map(c -> new RankDtos.BadgeCatalogItem(
            c.getCode(),
            c.getName(),
            c.getRarity().name(),
            earnedIds.contains(c.getId())
        ))
        .toList();

    BadgeStats stats = toStats(catalog, earnedIds);
    return new RankDtos.BadgeStatusResponse(summaries, catalogItems, stats);
  }

  @Transactional
  public RankDtos.BadgeStatusResponse evaluate(String userId) {
    try {
      Map<String, BadgeCatalog> catalogByCode = badgeCatalogRepository.findAll().stream()
          .collect(Collectors.toMap(BadgeCatalog::getCode, c -> c));
      Set<Long> alreadyEarned = userBadgeRepository.findByUserIdOrderByEarnedAtDesc(userId).stream()
          .map(UserBadge::getBadgeId)
          .collect(Collectors.toSet());

      List<BadgeCatalog> newlyEarned = new ArrayList<>();

      maybeAward(userId, "FIRST_SOLVE", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userAnswerRepository.countByUserId(userId) >= 1;
            } catch (Exception e) {
              log.warn("Failed to check FIRST_SOLVE badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      maybeAward(userId, "STREAK_BRONZE", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userStreakRepository.findById(userId)
                  .map(UserStreak::getCurrentDays)
                  .orElse(0) >= 7;
            } catch (Exception e) {
              log.warn("Failed to check STREAK_BRONZE badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      maybeAward(userId, "XP_MASTER", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userXpWalletRepository.findById(userId)
                  .map(UserXpWallet::getXpTotal)
                  .orElse(0L) >= 3_000;
            } catch (Exception e) {
              log.warn("Failed to check XP_MASTER badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      maybeAward(userId, "WEEKLY_WARRIOR", catalogByCode, alreadyEarned,
          () -> {
            try {
              return reportWeeklyRepository.findTopByUserIdOrderByWeekIsoDesc(userId)
                  .map(ReportWeekly::getSolvedCount)
                  .orElse(0) >= 70;
            } catch (Exception e) {
              log.warn("Failed to check WEEKLY_WARRIOR badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      maybeAward(userId, "TAG_SPECIALIST", catalogByCode, alreadyEarned,
          () -> {
            try {
              return reportTagSkillRepository.findByUserIdOrderByAccuracyDesc(userId).stream()
                  .anyMatch(skill -> skill.getAccuracy() != null 
                      && skill.getAccuracy().doubleValue() >= 90.0 
                      && skill.getTotal() >= 20);
            } catch (Exception e) {
              log.warn("Failed to check TAG_SPECIALIST badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      if (!newlyEarned.isEmpty()) {
        List<UserBadge> badges = newlyEarned.stream()
            .map(b -> UserBadge.builder()
                .userId(userId)
                .badgeId(b.getId())
                .earnedAt(Instant.now())
                .build())
            .toList();
        userBadgeRepository.saveAll(badges);
      }

      return status(userId);
    } catch (Exception e) {
      log.error("Failed to evaluate badges for user {}: {}", userId, e.getMessage(), e);
      // 에러 발생 시에도 현재 상태는 반환
      return status(userId);
    }
  }

  private BadgeStats toStats(List<BadgeCatalog> catalog, Set<Long> earnedIds) {
    Map<String, Long> totalByRarity = catalog.stream()
        .collect(Collectors.groupingBy(c -> c.getRarity().name(), Collectors.counting()));
    Map<String, Long> earnedByRarity = catalog.stream()
        .filter(c -> earnedIds.contains(c.getId()))
        .collect(Collectors.groupingBy(c -> c.getRarity().name(), Collectors.counting()));

    List<BadgeRarityStat> rarityStats = totalByRarity.entrySet().stream()
        .map(entry -> {
          String rarity = entry.getKey();
          int total = entry.getValue().intValue();
          int earned = earnedByRarity.getOrDefault(rarity, 0L).intValue();
          return new BadgeRarityStat(rarity, earned, total);
        })
        .sorted(Comparator.comparing(BadgeRarityStat::rarity))
        .toList();

    return new BadgeStats(earnedIds.size(), catalog.size(), rarityStats);
  }

  private void maybeAward(String userId,
                          String code,
                          Map<String, BadgeCatalog> catalogByCode,
                          Set<Long> alreadyEarned,
                          Condition condition,
                          List<BadgeCatalog> newlyEarned) {
    BadgeCatalog catalog = catalogByCode.get(code);
    if (catalog == null) {
      log.warn("Badge catalog for code {} not found", code);
      return;
    }
    if (alreadyEarned.contains(catalog.getId())) {
      return;
    }
    if (condition.check()) {
      newlyEarned.add(catalog);
    }
  }

  private String writeJson(Object value) {
    try {
      return objectMapper.writeValueAsString(value);
    } catch (JsonProcessingException e) {
      log.warn("Failed to serialize badge rule: {}", e.getMessage());
      return "{}";
    }
  }

  @FunctionalInterface
  private interface Condition {
    boolean check();
  }

  private record BadgeSeed(
      String code,
      String name,
      Rarity rarity,
      Map<String, Object> rule
  ) {}
}

