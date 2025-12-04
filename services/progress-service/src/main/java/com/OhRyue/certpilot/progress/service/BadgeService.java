package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.*;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.MainType;
import com.OhRyue.certpilot.progress.domain.enums.Rarity;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos.BadgeRarityStat;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos.BadgeStats;
import com.OhRyue.certpilot.progress.event.BadgeEarnedEvent;
import com.OhRyue.certpilot.progress.repository.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class BadgeService {

  // 10개 배지만 사용하므로 DEFAULT_BADGES 비활성화
  // V6 마이그레이션에서 등록한 10개 배지만 사용
  private static final List<BadgeSeed> DEFAULT_BADGES = List.of();

  private final BadgeCatalogRepository badgeCatalogRepository;
  private final UserBadgeRepository userBadgeRepository;
  private final UserStreakRepository userStreakRepository;
  private final UserXpWalletRepository userXpWalletRepository;
  private final UserSkillCounterRepository userSkillCounterRepository;
  private final ProgressActivityRepository progressActivityRepository;
  private final ApplicationEventPublisher eventPublisher;
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

  // 10개 공식 배지 코드 목록
  private static final Set<String> OFFICIAL_BADGE_CODES = Set.of(
      "FIRST_STUDY",
      "CONSISTENT_3DAYS",
      "ACCURACY_MASTER",
      "WRITTEN_EXPERT",
      "PRACTICAL_PERFECT",
      "DUEL_STREAK_3",
      "TOURNAMENT_WINNER",
      "GOLDENBELL_WINNER",
      "CONSISTENT_7DAYS",
      "XP_10000"
  );

  @Transactional(readOnly = true)
  public RankDtos.BadgeStatusResponse status(String userId) {
    // 10개 공식 배지만 조회
    List<BadgeCatalog> allCatalogs = badgeCatalogRepository.findAll();
    List<BadgeCatalog> catalog = allCatalogs.stream()
        .filter(c -> c.getCode() != null && OFFICIAL_BADGE_CODES.contains(c.getCode()))
        .toList();
    
    List<UserBadge> earned = userBadgeRepository.findByUserIdOrderByEarnedAtDesc(userId);
    Set<Long> earnedIds = earned.stream().map(UserBadge::getBadgeId).collect(Collectors.toSet());
    
    // 10개 배지 중 획득한 배지만 필터링
    Set<Long> officialBadgeIds = catalog.stream()
        .map(BadgeCatalog::getId)
        .collect(Collectors.toSet());
    
    List<RankDtos.BadgeSummary> summaries = earned.stream()
        .filter(b -> officialBadgeIds.contains(b.getBadgeId()))  // 10개 배지만 포함
        .map(b -> {
          BadgeCatalog meta = catalog.stream()
              .filter(c -> c.getId().equals(b.getBadgeId()))
              .findFirst()
              .orElse(null);
          if (meta == null || meta.getRarity() == null) {
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
        .filter(c -> c.getRarity() != null)  // rarity가 null인 배지는 제외
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

  public RankDtos.BadgeStatusResponse evaluate(String userId) {
    log.info("Starting badge evaluation for user: {}", userId);
    
    // 트랜잭션 내에서 배지 평가 및 저장 (10개 배지만 체크)
    try {
      evaluateAndAwardBadges(userId);
    } catch (Exception e) {
      log.error("Failed to evaluate and award badges for user {}: {}", userId, e.getMessage(), e);
    }
    
    // 트랜잭션 밖에서 상태 조회 (readOnly 트랜잭션 충돌 방지)
    return getStatusSafely(userId);
  }
  
  
  @Transactional
  private void evaluateAndAwardBadges(String userId) {
    try {
      // code가 null인 배지는 제외, 중복 키가 있으면 첫 번째 것만 사용
      List<BadgeCatalog> allCatalogs = badgeCatalogRepository.findAll();
      log.debug("Found {} badge catalogs in database", allCatalogs.size());
      
      Map<String, BadgeCatalog> catalogByCode = allCatalogs.stream()
          .filter(c -> c.getCode() != null && !c.getCode().isEmpty())
          .collect(Collectors.toMap(
              BadgeCatalog::getCode, 
              c -> c,
              (existing, replacement) -> {
                log.warn("Duplicate badge code found: {}, keeping existing", existing.getCode());
                return existing;
              }
          ));
      log.debug("Created catalog map with {} badges", catalogByCode.size());
      
      List<UserBadge> earnedBadges = userBadgeRepository.findByUserIdOrderByEarnedAtDesc(userId);
      log.debug("Found {} already earned badges for user {}", earnedBadges.size(), userId);
      
      Set<Long> alreadyEarned = earnedBadges.stream()
          .map(UserBadge::getBadgeId)
          .collect(Collectors.toSet());

      List<BadgeCatalog> newlyEarned = new ArrayList<>();

      // 10개 배지만 체크 (V6 마이그레이션에서 등록한 공식 배지)
      log.debug("Checking skill counter for user {}", userId);
      UserSkillCounter skillCounter = userSkillCounterRepository.findByUserId(userId)
          .orElseGet(() -> {
            log.debug("Creating new skill counter for user {}", userId);
            return UserSkillCounter.builder()
                .userId(userId)
                .firstStudyCompleted(false)
                .writtenReview90Cnt(0)
                .practicalMicro100Cnt(0)
                .accuracy80Cnt(0)
                .duelStreak(0)
                .tournamentWins(0)
                .goldenbellWins(0)
                .build();
          });

      // 1. FIRST_STUDY
      maybeAward(userId, "FIRST_STUDY", catalogByCode, alreadyEarned,
          () -> skillCounter.getFirstStudyCompleted(),
          newlyEarned);

      // 2. CONSISTENT_3DAYS
      maybeAward(userId, "CONSISTENT_3DAYS", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userStreakRepository.findById(userId)
                  .map(UserStreak::getCurrentDays)
                  .orElse(0) >= 3;
            } catch (Exception e) {
              log.warn("Failed to check CONSISTENT_3DAYS badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      // 3. ACCURACY_MASTER - progress_activity에서 정확도 확인
      maybeAward(userId, "ACCURACY_MASTER", catalogByCode, alreadyEarned,
          () -> {
            try {
              // progress_activity에서 정답률 80% 이상 활동 개수 확인
              long count = progressActivityRepository.countByUserIdAndAccuracyPctGreaterThanEqual80(userId);
              // skillCounter도 업데이트 (동기화)
              if (count >= 10 && skillCounter.getAccuracy80Cnt() < 10) {
                try {
                  skillCounter.setAccuracy80Cnt((int) count);
                  userSkillCounterRepository.save(skillCounter);
                } catch (Exception saveEx) {
                  log.warn("Failed to save skill counter for user {}: {}", userId, saveEx.getMessage());
                  // 저장 실패해도 배지 체크는 계속 진행
                }
              }
              return count >= 10;
            } catch (Exception e) {
              log.warn("Failed to check ACCURACY_MASTER badge for user {}: {}", userId, e.getMessage());
              return skillCounter.getAccuracy80Cnt() >= 10;
            }
          },
          newlyEarned);

      // 4. WRITTEN_EXPERT - progress_activity에서 필기 REVIEW 90점 이상 확인
      maybeAward(userId, "WRITTEN_EXPERT", catalogByCode, alreadyEarned,
          () -> {
            try {
              List<ProgressActivity> activities = progressActivityRepository
                  .findByUserIdAndExamModeAndMainType(userId, ExamMode.WRITTEN, MainType.REVIEW);
              
              long count = activities.stream()
                  .filter(a -> a.getAccuracyPct() != null && a.getAccuracyPct().doubleValue() >= 90.0)
                  .count();
              
              // skillCounter도 업데이트 (동기화)
              if (count >= 5 && skillCounter.getWrittenReview90Cnt() < 5) {
                try {
                  skillCounter.setWrittenReview90Cnt((int) count);
                  userSkillCounterRepository.save(skillCounter);
                } catch (Exception saveEx) {
                  log.warn("Failed to save skill counter for user {}: {}", userId, saveEx.getMessage());
                  // 저장 실패해도 배지 체크는 계속 진행
                }
              }
              
              return count >= 5;
            } catch (Exception e) {
              log.warn("Failed to check WRITTEN_EXPERT badge for user {}: {}", userId, e.getMessage());
              return skillCounter.getWrittenReview90Cnt() >= 5;
            }
          },
          newlyEarned);

      // 5. PRACTICAL_PERFECT - progress_activity에서 실기 MICRO 100점 확인
      maybeAward(userId, "PRACTICAL_PERFECT", catalogByCode, alreadyEarned,
          () -> {
            try {
              List<ProgressActivity> activities = progressActivityRepository
                  .findByUserIdAndExamModeAndMainType(userId, ExamMode.PRACTICAL, MainType.MICRO);
              
              long count = activities.stream()
                  .filter(a -> a.getAccuracyPct() != null && a.getAccuracyPct().doubleValue() >= 100.0)
                  .count();
              
              // skillCounter도 업데이트 (동기화)
              if (count >= 3 && skillCounter.getPracticalMicro100Cnt() < 3) {
                try {
                  skillCounter.setPracticalMicro100Cnt((int) count);
                  userSkillCounterRepository.save(skillCounter);
                } catch (Exception saveEx) {
                  log.warn("Failed to save skill counter for user {}: {}", userId, saveEx.getMessage());
                  // 저장 실패해도 배지 체크는 계속 진행
                }
              }
              
              return count >= 3;
            } catch (Exception e) {
              log.warn("Failed to check PRACTICAL_PERFECT badge for user {}: {}", userId, e.getMessage());
              return skillCounter.getPracticalMicro100Cnt() >= 3;
            }
          },
          newlyEarned);

      // 6. DUEL_STREAK_3
      maybeAward(userId, "DUEL_STREAK_3", catalogByCode, alreadyEarned,
          () -> skillCounter.getDuelStreak() >= 3,
          newlyEarned);

      // 7. TOURNAMENT_WINNER
      maybeAward(userId, "TOURNAMENT_WINNER", catalogByCode, alreadyEarned,
          () -> skillCounter.getTournamentWins() >= 1,
          newlyEarned);

      // 8. GOLDENBELL_WINNER
      maybeAward(userId, "GOLDENBELL_WINNER", catalogByCode, alreadyEarned,
          () -> skillCounter.getGoldenbellWins() >= 1,
          newlyEarned);

      // 9. CONSISTENT_7DAYS
      maybeAward(userId, "CONSISTENT_7DAYS", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userStreakRepository.findById(userId)
                  .map(UserStreak::getCurrentDays)
                  .orElse(0) >= 7;
            } catch (Exception e) {
              log.warn("Failed to check CONSISTENT_7DAYS badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      // 10. XP_10000
      maybeAward(userId, "XP_10000", catalogByCode, alreadyEarned,
          () -> {
            try {
              return userXpWalletRepository.findById(userId)
                  .map(UserXpWallet::getXpTotal)
                  .orElse(0L) >= 10_000;
            } catch (Exception e) {
              log.warn("Failed to check XP_10000 badge for user {}: {}", userId, e.getMessage());
              return false;
            }
          },
          newlyEarned);

      if (!newlyEarned.isEmpty()) {
        Instant earnedAt = Instant.now();
        List<UserBadge> badges = newlyEarned.stream()
            .filter(b -> b.getId() != null)  // id가 null인 배지는 제외
            .map(b -> UserBadge.builder()
                .userId(userId)
                .badgeId(b.getId())
                .earnedAt(earnedAt)
                .build())
            .toList();
        
        if (!badges.isEmpty()) {
          try {
            userBadgeRepository.saveAll(badges);
            log.info("Saved {} new badges for user {}", badges.size(), userId);
          } catch (Exception saveEx) {
            log.error("Failed to save badges for user {}: {}", userId, saveEx.getMessage(), saveEx);
            // 저장 실패해도 상태는 반환
          }
        }
        
        // 배지 지급 이벤트 발행
        for (BadgeCatalog badge : newlyEarned) {
          try {
            if (badge.getCode() == null || badge.getName() == null) {
              log.warn("Badge has null code or name, skipping event: badgeId={}", badge.getId());
              continue;
            }
            
            eventPublisher.publishEvent(new BadgeEarnedEvent(
                this,
                userId,
                badge.getCode(),
                badge.getName(),
                badge.getCategory() != null ? badge.getCategory() : "UNKNOWN",  // null 체크
                earnedAt
            ));
            log.info("Badge earned event published: userId={}, badgeCode={}, badgeName={}", 
                userId, badge.getCode(), badge.getName());
          } catch (Exception e) {
            log.warn("Failed to publish badge earned event for user {} badge {}: {}", 
                userId, badge.getCode() != null ? badge.getCode() : "unknown", e.getMessage());
          }
        }
      }

      log.info("Badge evaluation completed for user {}, {} new badges earned", userId, newlyEarned.size());
    } catch (Exception e) {
      log.error("Failed to evaluate badges for user {}: {}", userId, e.getMessage(), e);
      log.error("Exception type: {}, Stack trace: ", e.getClass().getName(), e);
      // 에러가 발생해도 트랜잭션은 롤백되지 않도록 (이미 저장된 배지는 유지)
      // 예외를 다시 던지지 않고 로그만 남김
    }
  }
  
  /**
   * 안전하게 배지 상태 조회 (트랜잭션 밖에서 호출)
   */
  private RankDtos.BadgeStatusResponse getStatusSafely(String userId) {
    try {
      return status(userId);
    } catch (Exception statusEx) {
      log.error("Failed to get badge status for user {}: {}", userId, statusEx.getMessage(), statusEx);
      log.error("Status exception type: {}, Stack trace: ", statusEx.getClass().getName(), statusEx);
      // 최소한의 응답 반환
      return new RankDtos.BadgeStatusResponse(
          List.of(),
          List.of(),
          new RankDtos.BadgeStats(0, 0, List.of())
      );
    }
  }

  private BadgeStats toStats(List<BadgeCatalog> catalog, Set<Long> earnedIds) {
    // rarity가 null인 배지는 제외
    List<BadgeCatalog> validCatalog = catalog.stream()
        .filter(c -> c.getRarity() != null)
        .toList();
    
    Map<String, Long> totalByRarity = validCatalog.stream()
        .collect(Collectors.groupingBy(c -> c.getRarity().name(), Collectors.counting()));
    Map<String, Long> earnedByRarity = validCatalog.stream()
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

    return new BadgeStats(earnedIds.size(), validCatalog.size(), rarityStats);
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

  /**
   * 학습 완료 시 skill counter 업데이트 및 배지 체크
   * accuracyPct가 null이면 progress_activity에서 최근 활동을 조회하여 확인
   */
  @Transactional
  public void updateSkillCounterOnStudyComplete(String userId, String examMode, String flowType, 
                                                 Double accuracyPct) {
    try {
      // accuracyPct가 null이면 progress_activity에서 정확한 활동 조회
      if (accuracyPct == null && examMode != null && flowType != null) {
        try {
          ExamMode mode = ExamMode.valueOf(examMode.toUpperCase());
          MainType mainType = MainType.valueOf(flowType.toUpperCase());
          
          List<ProgressActivity> activities = progressActivityRepository
              .findByUserIdAndExamModeAndMainType(userId, mode, mainType);
          
          // 가장 최근 활동의 정확도 사용
          if (!activities.isEmpty()) {
            ProgressActivity latest = activities.get(0);
            accuracyPct = latest.getAccuracyPct() != null 
                ? latest.getAccuracyPct().doubleValue() 
                : null;
          }
        } catch (Exception e) {
          log.warn("Failed to query progress_activity for user {}: {}", userId, e.getMessage());
          // fallback: 최근 활동 조회
          List<ProgressActivity> recentActivities = progressActivityRepository
              .findTop5ByUserIdOrderByFinishedAtDesc(userId);
          
          for (ProgressActivity activity : recentActivities) {
            if (activity.getMode() != null && 
                activity.getMode().name().equalsIgnoreCase(examMode) &&
                activity.getMainType() != null &&
                activity.getMainType().name().equalsIgnoreCase(flowType)) {
              accuracyPct = activity.getAccuracyPct() != null 
                  ? activity.getAccuracyPct().doubleValue() 
                  : null;
              break;
            }
          }
        }
      }

      UserSkillCounter counter = userSkillCounterRepository.findByUserId(userId)
          .orElseGet(() -> UserSkillCounter.builder()
              .userId(userId)
              .firstStudyCompleted(false)
              .writtenReview90Cnt(0)
              .practicalMicro100Cnt(0)
              .accuracy80Cnt(0)
              .duelStreak(0)
              .tournamentWins(0)
              .goldenbellWins(0)
              .build());

      // 첫 학습 완료 체크
      if (!counter.getFirstStudyCompleted() && 
          (flowType != null && (flowType.equals("MICRO") || flowType.equals("REVIEW")))) {
        counter.setFirstStudyCompleted(true);
      }

      // 필기 REVIEW 90점 이상 체크 (progress_activity에서도 확인하여 동기화)
      if ("WRITTEN".equalsIgnoreCase(examMode) && "REVIEW".equalsIgnoreCase(flowType) 
          && accuracyPct != null && accuracyPct >= 90.0) {
        try {
          // progress_activity에서 필기 REVIEW 90점 이상 개수 확인
          List<ProgressActivity> activities = progressActivityRepository
              .findByUserIdAndExamModeAndMainType(userId, ExamMode.WRITTEN, MainType.REVIEW);
          
          long count = activities.stream()
              .filter(a -> a.getAccuracyPct() != null && a.getAccuracyPct().doubleValue() >= 90.0)
              .count();
          
          counter.setWrittenReview90Cnt((int) Math.max(counter.getWrittenReview90Cnt(), count));
        } catch (Exception e) {
          log.warn("Failed to sync writtenReview90Cnt from progress_activity for user {}: {}", userId, e.getMessage());
          // 실패 시 기존 카운터만 증가
          counter.setWrittenReview90Cnt(counter.getWrittenReview90Cnt() + 1);
        }
      }

      // 실기 MICRO 100점 체크 (progress_activity에서도 확인하여 동기화)
      if ("PRACTICAL".equalsIgnoreCase(examMode) && "MICRO".equalsIgnoreCase(flowType) 
          && accuracyPct != null && accuracyPct >= 100.0) {
        try {
          // progress_activity에서 실기 MICRO 100점 개수 확인
          List<ProgressActivity> activities = progressActivityRepository
              .findByUserIdAndExamModeAndMainType(userId, ExamMode.PRACTICAL, MainType.MICRO);
          
          long count = activities.stream()
              .filter(a -> a.getAccuracyPct() != null && a.getAccuracyPct().doubleValue() >= 100.0)
              .count();
          
          counter.setPracticalMicro100Cnt((int) Math.max(counter.getPracticalMicro100Cnt(), count));
        } catch (Exception e) {
          log.warn("Failed to sync practicalMicro100Cnt from progress_activity for user {}: {}", userId, e.getMessage());
          // 실패 시 기존 카운터만 증가
          counter.setPracticalMicro100Cnt(counter.getPracticalMicro100Cnt() + 1);
        }
      }

      // 정답률 80% 이상 체크 (progress_activity에서도 확인)
      if (accuracyPct != null && accuracyPct >= 80.0) {
        // progress_activity에서 정답률 80% 이상 활동 개수 확인하여 동기화
        try {
          long count = progressActivityRepository.countByUserIdAndAccuracyPctGreaterThanEqual80(userId);
          counter.setAccuracy80Cnt((int) Math.max(counter.getAccuracy80Cnt(), count));
        } catch (Exception e) {
          log.warn("Failed to sync accuracy80Cnt from progress_activity for user {}: {}", userId, e.getMessage());
          // 실패 시 기존 카운터만 증가
          counter.setAccuracy80Cnt(counter.getAccuracy80Cnt() + 1);
        }
      }

      userSkillCounterRepository.save(counter);
      
      // 배지 평가
      evaluate(userId);
    } catch (Exception e) {
      log.error("Failed to update skill counter for user {}: {}", userId, e.getMessage(), e);
    }
  }

  /**
   * 대전 결과에 따른 skill counter 업데이트 및 배지 체크
   */
  @Transactional
  public void updateSkillCounterOnVersusResult(String userId, String mode, boolean isWinner) {
    try {
      UserSkillCounter counter = userSkillCounterRepository.findByUserId(userId)
          .orElseGet(() -> UserSkillCounter.builder()
              .userId(userId)
              .firstStudyCompleted(false)
              .writtenReview90Cnt(0)
              .practicalMicro100Cnt(0)
              .accuracy80Cnt(0)
              .duelStreak(0)
              .tournamentWins(0)
              .goldenbellWins(0)
              .build());

      if ("DUEL".equalsIgnoreCase(mode)) {
        if (isWinner) {
          counter.setDuelStreak(counter.getDuelStreak() + 1);
        } else {
          counter.setDuelStreak(0); // 연승 끊김
        }
      } else if ("TOURNAMENT".equalsIgnoreCase(mode) && isWinner) {
        counter.setTournamentWins(counter.getTournamentWins() + 1);
      } else if ("GOLDENBELL".equalsIgnoreCase(mode) && isWinner) {
        counter.setGoldenbellWins(counter.getGoldenbellWins() + 1);
      }

      userSkillCounterRepository.save(counter);
      
      // 배지 평가
      evaluate(userId);
    } catch (Exception e) {
      log.error("Failed to update skill counter for versus result for user {}: {}", userId, e.getMessage(), e);
    }
  }

  private record BadgeSeed(
      String code,
      String name,
      Rarity rarity,
      Map<String, Object> rule
  ) {}
}

