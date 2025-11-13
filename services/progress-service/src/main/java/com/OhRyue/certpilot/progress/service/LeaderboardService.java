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
                                                  String reference) {
    if (userId != null && (scope == RankScope.OVERALL || scope == RankScope.FRIENDS)) {
      rankService.recomputeForUser(userId);
    }
    return switch (scope) {
      case OVERALL -> overallLeaderboard(userId);
      case WEEKLY -> weeklyLeaderboard(userId, reference);
      case HALL_OF_FAME -> hallOfFameLeaderboard(userId, reference);
      case FRIENDS -> friendsLeaderboard(userId);
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

  private RankDtos.LeaderboardResponse overallLeaderboard(String userId) {
    List<UserRankScore> top = rankService.topN(DEFAULT_LEADERBOARD_LIMIT);
    List<RankDtos.LeaderboardEntry> entries = enrichOverallEntries(top);

    RankDtos.LeaderboardEntry meEntry = computeOverallEntry(userId);
    String reference = formatReference(RankScope.OVERALL, LocalDate.now());
    Instant generatedAt = Instant.now();

    return new RankDtos.LeaderboardResponse(RankScope.OVERALL, reference, generatedAt, entries, meEntry);
  }

  private List<RankDtos.LeaderboardEntry> enrichOverallEntries(List<UserRankScore> scores) {
    List<String> userIds = scores.stream().map(UserRankScore::getUserId).toList();

    Map<String, UserXpWallet> wallets = xpWalletRepository.findAllById(userIds).stream()
        .collect(Collectors.toMap(UserXpWallet::getUserId, w -> w));
    Map<String, UserStreak> streaks = userStreakRepository.findAllById(userIds).stream()
        .collect(Collectors.toMap(UserStreak::getUserId, s -> s));

    int rank = 1;
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

  private RankDtos.LeaderboardResponse weeklyLeaderboard(String userId, String reference) {
    LocalDate refDate = resolveReferenceDate(RankScope.WEEKLY, reference);
    String weekIso = weekKey(refDate);
    List<RankDtos.LeaderboardEntry> entries = weeklyEntries(weekIso, DEFAULT_LEADERBOARD_LIMIT);

    RankDtos.LeaderboardEntry meEntry = null;
    if (userId != null) {
      meEntry = reportWeeklyRepository.findByUserIdAndWeekIso(userId, weekIso)
          .map(report -> {
            long higher = reportWeeklyRepository.findByWeekIsoOrderByXpGainedDesc(weekIso, Pageable.unpaged()).stream()
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

    return new RankDtos.LeaderboardResponse(RankScope.WEEKLY, weekIso, Instant.now(), entries, meEntry);
  }

  private RankDtos.LeaderboardResponse hallOfFameLeaderboard(String userId, String reference) {
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

    return new RankDtos.LeaderboardResponse(RankScope.HALL_OF_FAME, formatReference(RankScope.HALL_OF_FAME, LocalDate.now()), Instant.now(), entries, meEntry);
  }

  private RankDtos.LeaderboardResponse friendsLeaderboard(String userId) {
    List<UserFriend> friends = userFriendRepository.findByUserIdAndStatus(userId, FRIEND_ACCEPTED);
    List<String> candidateIds = new ArrayList<>();
    candidateIds.add(userId);
    candidateIds.addAll(friends.stream().map(UserFriend::getFriendId).toList());

    candidateIds.forEach(rankService::recomputeForUser);

    List<UserRankScore> scores = rankScoreRepository.findByUserIdInOrderByScoreDesc(candidateIds);
    List<RankDtos.LeaderboardEntry> entries = enrichOverallEntries(scores);
    RankDtos.LeaderboardEntry meEntry = entries.stream()
        .filter(entry -> entry.userId().equals(userId))
        .findFirst()
        .orElse(null);

    return new RankDtos.LeaderboardResponse(RankScope.FRIENDS, formatReference(RankScope.FRIENDS, LocalDate.now()), Instant.now(), entries, meEntry);
  }

  private List<RankDtos.LeaderboardEntry> generateSnapshot(RankScope scope, LocalDate refDate) {
    return switch (scope) {
      case OVERALL -> enrichOverallEntries(rankScoreRepository.findAllByOrderByScoreDesc(PageRequest.of(0, DEFAULT_LEADERBOARD_LIMIT)));
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
      return objectMapper.readValue(payloadJson, new TypeReference<>() {});
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
      case WEEKLY -> parseWeek(reference);
    };
  }

  private String formatReference(RankScope scope, LocalDate date) {
    return switch (scope) {
      case OVERALL, HALL_OF_FAME, FRIENDS -> date.format(DateTimeFormatter.ISO_DATE);
      case WEEKLY -> weekKey(date);
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
    int year = Integer.parseInt(parts[0]);
    int week = Integer.parseInt(parts[1]);
    return LocalDate.now()
        .with(weekFields.weekBasedYear(), year)
        .with(weekFields.weekOfWeekBasedYear(), week)
        .with(weekFields.dayOfWeek(), 1);
  }
}

