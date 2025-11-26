package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.*;
import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import com.OhRyue.certpilot.progress.dto.rank.RankDtos;
import com.OhRyue.certpilot.progress.repository.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.WeekFields;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class LeaderboardService {

  private static final int DEFAULT_LEADERBOARD_LIMIT = 20;
  private static final String FRIEND_ACCEPTED = "ACCEPTED";

  private final UserRankScoreRepository rankScoreRepository;
  private final UserXpWalletRepository xpWalletRepository;
  private final UserStreakRepository userStreakRepository;
  private final ReportWeeklyRepository reportWeeklyRepository;
  private final UserFriendRepository userFriendRepository;
  private final LeaderboardSnapshotRepository snapshotRepository;
  private final ObjectMapper objectMapper;
  private final RankService rankService;

  @Transactional(readOnly = true)
  public RankDtos.LeaderboardResponse leaderboard(RankScope scope,
                                                  String userId,
                                                  String reference,
                                                  Integer page,
                                                  Integer size) {
    int pageSafe = (page == null || page < 0) ? 0 : page;
    int sizeSafe;
    if (size == null || size <= 0) {
      sizeSafe = DEFAULT_LEADERBOARD_LIMIT;
    } else {
      sizeSafe = Math.min(size, 100); // 상한선 방어
    }

    if (userId != null && (scope == RankScope.OVERALL || scope == RankScope.FRIENDS)) {
      rankService.recomputeForUser(userId);
    }

    return switch (scope) {
      case OVERALL -> overallLeaderboard(userId, pageSafe, sizeSafe);
      case WEEKLY -> weeklyLeaderboard(userId, reference, pageSafe, sizeSafe);
      case HALL_OF_FAME -> hallOfFameLeaderboard(userId, reference, pageSafe, sizeSafe);
      case FRIENDS -> friendsLeaderboard(userId, pageSafe, sizeSafe);
    };
  }

  @Transactional(readOnly = true)
  public RankDtos.LeaderboardHistoryResponse history(RankScope scope, String reference) {
    LocalDate refDate = resolveReferenceDate(scope, reference);
    Optional<LeaderboardSnapshot> snapshotOpt = snapshotRepository.findByScopeAndSnapshotDate(scope, refDate);
    if (snapshotOpt.isEmpty()) {
      List<RankDtos.LeaderboardEntry> generated = generateSnapshot(scope, refDate);
      return new RankDtos.LeaderboardHistoryResponse(scope, formatReference(scope, refDate), Instant.now(), generated);
    }
    LeaderboardSnapshot snapshot = snapshotOpt.get();
    List<RankDtos.LeaderboardEntry> entries = deserializeEntries(snapshot.getPayloadJson());
    Instant generatedAt = snapshot.getSnapshotDate()
        .atStartOfDay(ZoneId.systemDefault())
        .toInstant();
    return new RankDtos.LeaderboardHistoryResponse(scope, formatReference(scope, refDate), generatedAt, entries);
  }

  @Transactional
  public void recompute(RankScope scope, String reference) {
    LocalDate refDate = resolveReferenceDate(scope, reference);
    List<RankDtos.LeaderboardEntry> entries = generateSnapshot(scope, refDate);
    try {
      String payload = objectMapper.writeValueAsString(entries);
      LeaderboardSnapshot snapshot = snapshotRepository.findByScopeAndSnapshotDate(scope, refDate)
          .orElseGet(() -> LeaderboardSnapshot.builder()
              .scope(scope)
              .snapshotDate(refDate)
              .payloadJson(payload)
              .build());
      snapshot.setPayloadJson(payload);
      snapshotRepository.save(snapshot);
    } catch (JsonProcessingException e) {
      log.error("Failed to persist leaderboard snapshot {} {}: {}", scope, reference, e.getMessage());
    }
  }

  /* =========================
   *  각 Scope별 Leaderboard
   * ========================= */

  private RankDtos.LeaderboardResponse overallLeaderboard(String userId, int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    List<UserRankScore> scores = rankScoreRepository.findAllByOrderByScoreDesc(pageable);

    long totalElements = rankScoreRepository.count();
    int totalPages = totalElements == 0 ? 0 : (int) ((totalElements + size - 1) / size);

    int rankOffset = page * size;
    List<RankDtos.LeaderboardEntry> entries = enrichOverallEntries(scores, rankOffset);

    RankDtos.LeaderboardEntry meEntry = computeOverallEntry(userId);
    String reference = formatReference(RankScope.OVERALL, LocalDate.now());
    Instant generatedAt = Instant.now();

    return new RankDtos.LeaderboardResponse(
        RankScope.OVERALL,
        reference,
        generatedAt,
        entries,
        meEntry,
        page,
        size,
        totalElements,
        totalPages
    );
  }

  private List<RankDtos.LeaderboardEntry> enrichOverallEntries(List<UserRankScore> scores, int rankOffset) {
    List<String> userIds = scores.stream().map(UserRankScore::getUserId).toList();

    Map<String, UserXpWallet> wallets = xpWalletRepository.findAllById(userIds).stream()
        .collect(Collectors.toMap(UserXpWallet::getUserId, w -> w));
    Map<String, UserStreak> streaks = userStreakRepository.findAllById(userIds).stream()
        .collect(Collectors.toMap(UserStreak::getUserId, s -> s));

    int rank = rankOffset + 1;
    List<RankDtos.LeaderboardEntry> entries = new ArrayList<>();
    for (UserRankScore score : scores) {
      UserXpWallet wallet = wallets.get(score.getUserId());
      UserStreak streak = streaks.get(score.getUserId());
      entries.add(new RankDtos.LeaderboardEntry(
          score.getUserId(),
          score.getScore(),
          rank++,
          wallet == null ? null : wallet.getXpTotal(),
          streak == null ? null : streak.getBestDays(),
          score.getLastUpdatedAt()
      ));
    }
    return entries;
  }

  private RankDtos.LeaderboardEntry computeOverallEntry(String userId) {
    if (userId == null) {
      return null;
    }
    return rankScoreRepository.findById(userId)
        .map(score -> {
          long higher = rankScoreRepository.countByScoreGreaterThan(score.getScore());
          UserXpWallet wallet = xpWalletRepository.findById(userId).orElse(null);
          UserStreak streak = userStreakRepository.findById(userId).orElse(null);
          return new RankDtos.LeaderboardEntry(
              score.getUserId(),
              score.getScore(),
              (int) higher + 1,
              wallet == null ? null : wallet.getXpTotal(),
              streak == null ? null : streak.getBestDays(),
              score.getLastUpdatedAt()
          );
        })
        .orElse(null);
  }

  private RankDtos.LeaderboardResponse weeklyLeaderboard(String userId, String reference, int page, int size) {
    LocalDate refDate = resolveReferenceDate(RankScope.WEEKLY, reference);
    String weekIso = weekKey(refDate);
    
    // 전체 엔트리 수 계산
    long totalElements = reportWeeklyRepository.findByWeekIsoOrderByXpGainedDesc(weekIso, Pageable.unpaged()).size();
    int totalPages = totalElements == 0 ? 0 : (int) ((totalElements + size - 1) / size);
    
    // 페이지네이션 적용
    List<RankDtos.LeaderboardEntry> entries = weeklyEntries(weekIso, page, size);

    RankDtos.LeaderboardEntry meEntry = null;
    if (userId != null) {
      meEntry = reportWeeklyRepository.findByUserIdAndWeekIso(userId, weekIso)
          .map(report -> {
            long higher = reportWeeklyRepository
                .findByWeekIsoOrderByXpGainedDesc(weekIso, Pageable.unpaged()).stream()
                .filter(r -> r.getXpGained() > report.getXpGained())
                .count();
            return new RankDtos.LeaderboardEntry(
                report.getUserId(),
                report.getXpGained(),
                (int) higher + 1,
                (long) report.getXpGained(),
                null,
                Instant.now()
            );
          })
          .orElse(null);
    }

    return new RankDtos.LeaderboardResponse(
        RankScope.WEEKLY,
        weekIso,
        Instant.now(),
        entries,
        meEntry,
        page,
        size,
        totalElements,
        totalPages
    );
  }

  @SuppressWarnings("unused")
  private RankDtos.LeaderboardResponse hallOfFameLeaderboard(String userId, String reference, int page, int size) {
    List<RankDtos.LeaderboardEntry> entries = hallEntries(3);

    RankDtos.LeaderboardEntry meEntry = null;
    if (userId != null) {
      meEntry = userStreakRepository.findById(userId)
          .map(streak -> {
            long higher = userStreakRepository.findTop10ByOrderByBestDaysDesc().stream()
                .filter(s -> s.getBestDays() > streak.getBestDays())
                .count();
            return new RankDtos.LeaderboardEntry(
                streak.getUserId(),
                streak.getBestDays(),
                (int) higher + 1,
                null,
                streak.getBestDays(),
                Instant.now()
            );
          })
          .orElse(null);
    }

    int effectiveSize = entries.size();
    long totalElements = effectiveSize;
    int totalPages = effectiveSize == 0 ? 0 : 1;

    return new RankDtos.LeaderboardResponse(
        RankScope.HALL_OF_FAME,
        formatReference(RankScope.HALL_OF_FAME, LocalDate.now()),
        Instant.now(),
        entries,
        meEntry,
        0,
        effectiveSize,
        totalElements,
        totalPages
    );
  }

  @SuppressWarnings("unused")
  private RankDtos.LeaderboardResponse friendsLeaderboard(String userId, int page, int size) {
    List<UserFriend> friends = userFriendRepository.findByUserIdAndStatus(userId, FRIEND_ACCEPTED);
    List<String> candidateIds = new ArrayList<>();
    candidateIds.add(userId);
    candidateIds.addAll(friends.stream().map(UserFriend::getFriendId).toList());

    candidateIds.forEach(rankService::recomputeForUser);

    List<UserRankScore> scores = rankScoreRepository.findByUserIdInOrderByScoreDesc(candidateIds);
    List<RankDtos.LeaderboardEntry> entries = enrichOverallEntries(scores, 0);
    RankDtos.LeaderboardEntry meEntry = entries.stream()
        .filter(entry -> entry.userId().equals(userId))
        .findFirst()
        .orElse(null);

    int effectiveSize = entries.size();
    long totalElements = effectiveSize;
    int totalPages = effectiveSize == 0 ? 0 : 1;

    return new RankDtos.LeaderboardResponse(
        RankScope.FRIENDS,
        formatReference(RankScope.FRIENDS, LocalDate.now()),
        Instant.now(),
        entries,
        meEntry,
        0,
        effectiveSize,
        totalElements,
        totalPages
    );
  }

  /* =========================
   *  Snapshot / 공통 유틸
   * ========================= */

  private List<RankDtos.LeaderboardEntry> generateSnapshot(RankScope scope, LocalDate refDate) {
    return switch (scope) {
      case OVERALL -> enrichOverallEntries(
          rankScoreRepository.findAllByOrderByScoreDesc(PageRequest.of(0, DEFAULT_LEADERBOARD_LIMIT)),
          0
      );
      case WEEKLY -> weeklyEntries(weekKey(refDate), DEFAULT_LEADERBOARD_LIMIT);
      case HALL_OF_FAME -> hallEntries(3);
      case FRIENDS -> List.of();
    };
  }

  private List<RankDtos.LeaderboardEntry> weeklyEntries(String weekIso, int limit) {
    Pageable pageable = PageRequest.of(0, limit);
    List<ReportWeekly> weekly = reportWeeklyRepository.findByWeekIsoOrderByXpGainedDesc(weekIso, pageable);
    List<RankDtos.LeaderboardEntry> entries = new ArrayList<>();
    int rank = 1;
    for (ReportWeekly report : weekly) {
      entries.add(new RankDtos.LeaderboardEntry(
          report.getUserId(),
          report.getXpGained(),
          rank++,
          (long) report.getXpGained(),
          null,
          Instant.now()
      ));
    }
    return entries;
  }

  private List<RankDtos.LeaderboardEntry> weeklyEntries(String weekIso, int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    List<ReportWeekly> weekly = reportWeeklyRepository.findByWeekIsoOrderByXpGainedDesc(weekIso, pageable);
    List<RankDtos.LeaderboardEntry> entries = new ArrayList<>();
    int rank = page * size + 1;
    for (ReportWeekly report : weekly) {
      entries.add(new RankDtos.LeaderboardEntry(
          report.getUserId(),
          report.getXpGained(),
          rank++,
          (long) report.getXpGained(),
          null,
          Instant.now()
      ));
    }
    return entries;
  }

  private List<RankDtos.LeaderboardEntry> hallEntries(int limit) {
    List<UserStreak> streaks = userStreakRepository.findTop10ByOrderByBestDaysDesc();
    List<RankDtos.LeaderboardEntry> entries = new ArrayList<>();
    int rank = 1;
    for (UserStreak streak : streaks.stream().limit(limit).toList()) {
      entries.add(new RankDtos.LeaderboardEntry(
          streak.getUserId(),
          streak.getBestDays(),
          rank++,
          null,
          streak.getBestDays(),
          Instant.now()
      ));
    }
    return entries;
  }

  private List<RankDtos.LeaderboardEntry> deserializeEntries(String payloadJson) {
    if (payloadJson == null) {
      return List.of();
    }
    try {
      return objectMapper.readValue(payloadJson, new TypeReference<>() {
      });
    } catch (JsonProcessingException e) {
      log.warn("Failed to deserialize leaderboard snapshot: {}", e.getMessage());
      return List.of();
    }
  }

  private LocalDate resolveReferenceDate(RankScope scope, String reference) {
    LocalDate today = LocalDate.now();
    if (reference == null || reference.isBlank()) {
      return today;
    }

    return switch (scope) {
      case OVERALL, HALL_OF_FAME, FRIENDS -> LocalDate.parse(reference);
      case WEEKLY -> {
        // 1) 2025-W47 같은 ISO week key 형식
        if (reference.contains("W")) {
          yield parseWeek(reference);
        }
        // 2) 2025-11-20 같은 날짜 형식도 허용
        yield LocalDate.parse(reference);
      }
    };
  }

  private String formatReference(RankScope scope, LocalDate date) {
    return switch (scope) {
      case OVERALL, HALL_OF_FAME, FRIENDS -> date.format(DateTimeFormatter.ISO_DATE);
      case WEEKLY -> weekKey(date); // 예: 2025-W47
    };
  }

  private String weekKey(LocalDate date) {
    WeekFields weekFields = WeekFields.ISO;
    int week = date.get(weekFields.weekOfWeekBasedYear());
    int year = date.get(weekFields.weekBasedYear());
    return "%d-W%02d".formatted(year, week);
  }

  private LocalDate parseWeek(String weekIso) {
    WeekFields weekFields = WeekFields.ISO;
    String[] parts = weekIso.split("-W");
    if (parts.length != 2) {
      // 방어 로직: 형식이 이상하면 그냥 오늘 기준으로 해석
      return LocalDate.now();
    }
    int year = Integer.parseInt(parts[0]);
    int week = Integer.parseInt(parts[1]);
    // 해당 주의 월요일(주 시작일)로 맞춰줌
    return LocalDate.now()
        .with(weekFields.weekBasedYear(), year)
        .with(weekFields.weekOfWeekBasedYear(), week)
        .with(weekFields.dayOfWeek(), 1);
  }
}
