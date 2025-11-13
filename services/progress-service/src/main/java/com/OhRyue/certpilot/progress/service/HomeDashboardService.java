package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.UserRankScore;
import com.OhRyue.certpilot.progress.domain.UserStreak;
import com.OhRyue.certpilot.progress.domain.UserXpWallet;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeGoal;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeOverview;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeProgressCard;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeQuickMenu;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeQuickMenuItem;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeQuickStats;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeRanking;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeRankingItem;
import com.OhRyue.certpilot.progress.dto.home.HomeDtos.HomeUserCard;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.feign.StudyReportClient;
import com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse;
import com.OhRyue.certpilot.progress.feign.dto.ProfileSummaryResponse;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.UserXpWalletRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class HomeDashboardService {

  private static final ZoneId KST = ZoneId.of("Asia/Seoul");

  private final AccountClient accountClient;
  private final StudyReportClient studyReportClient;
  private final XpService xpService;
  private final StreakService streakService;
  private final RankService rankService;
  private final ReportDailyRepository reportDailyRepository;
  private final UserXpWalletRepository userXpWalletRepository;

  public HomeOverview overview(String authorization, String userId) {
    AccountMeResponse me = accountClient.me(authorization);
    AccountMeResponse.Profile profile = me.profile();
    UserXpWallet wallet = xpService.getWallet(userId);
    UserStreak streak = streakService.get(userId);

    HomeUserCard userCard = new HomeUserCard(
        userId,
        profile == null ? null : profile.nickname(),
        profile == null ? null : profile.avatarUrl(),
        wallet.getLevel(),
        wallet.getXpTotal(),
        streak.getCurrentDays()
    );

    HomeGoal goal = null;
    if (me.goal() != null) {
      goal = new HomeGoal(
          me.goal().certId(),
          me.goal().targetExamMode(),
          me.goal().targetRoundId(),
          me.goal().ddayCached()
      );
    }

    return new HomeOverview(userCard, goal);
  }

  public HomeProgressCard progressCard(String authorization, String userId) {
    AccountMeResponse me = accountClient.me(authorization);
    if (me.goal() == null || me.goal().certId() == null) {
      return new HomeProgressCard(0, 0, 0, 0.0, null);
    }
    return studyReportClient.progressCard(userId, me.goal().certId());
  }

  public HomeRanking ranking(String authorization, String userId) {
    List<UserRankScore> top = rankService.topN(5);
    List<String> userIds = top.stream().map(UserRankScore::getUserId).collect(Collectors.toList());

    if (!userIds.contains(userId)) {
      userIds.add(userId);
    }

    List<ProfileSummaryResponse> summaries = userIds.isEmpty()
        ? List.of()
        : accountClient.summaries(authorization, userIds);
    Map<String, ProfileSummaryResponse> summaryMap = summaries.stream()
        .collect(Collectors.toMap(ProfileSummaryResponse::userId, s -> s, (a, b) -> a));

    Map<String, UserXpWallet> wallets = userXpWalletRepository.findAllById(userIds).stream()
        .collect(Collectors.toMap(UserXpWallet::getUserId, w -> w));

    List<HomeRankingItem> rankingItems = new ArrayList<>();
    int rank = 1;
    for (UserRankScore score : top) {
      ProfileSummaryResponse summary = summaryMap.get(score.getUserId());
      UserXpWallet wallet = wallets.getOrDefault(score.getUserId(), defaultWallet(score.getUserId()));
      rankingItems.add(new HomeRankingItem(
          score.getUserId(),
          summary == null ? null : summary.nickname(),
          summary == null ? null : summary.avatarUrl(),
          wallet.getLevel(),
          score.getScore(),
          wallet.getXpTotal(),
          score.getUserId().equals(userId),
          rank++
      ));
    }

    UserRankScore myScore = rankService.getScore(userId);
    HomeRankingItem meItem = null;
    if (myScore != null) {
      ProfileSummaryResponse summary = summaryMap.get(userId);
      UserXpWallet wallet = wallets.getOrDefault(userId, defaultWallet(userId));
      int myRank = rankService.resolveRank(myScore);
      meItem = new HomeRankingItem(
          userId,
          summary == null ? null : summary.nickname(),
          summary == null ? null : summary.avatarUrl(),
          wallet.getLevel(),
          myScore.getScore(),
          wallet.getXpTotal(),
          true,
          myRank
      );
    }

    rankingItems.sort(Comparator.comparingLong(HomeRankingItem::score).reversed());
    if (rankingItems.size() > 5) {
      rankingItems = rankingItems.subList(0, 5);
    }

    return new HomeRanking(rankingItems, meItem, Instant.now().toString());
  }

  public HomeQuickStats quickStats(String userId) {
    LocalDate today = LocalDate.now(KST);
    LocalDate yesterday = today.minusDays(1);

    ReportDaily todayReport = reportDailyRepository.findByUserIdAndDate(userId, today).orElse(null);
    ReportDaily yesterdayReport = reportDailyRepository.findByUserIdAndDate(userId, yesterday).orElse(null);

    int solved = todayReport == null ? 0 : todayReport.getSolvedCount();
    int minutes = todayReport == null ? 0 : todayReport.getTimeSpentSec() / 60;
    double accuracy = todayReport == null ? 0.0 : toDoublePercent(todayReport.getAccuracy());
    int xp = todayReport == null ? 0 : todayReport.getXpGained();

    double yesterdayAccuracy = yesterdayReport == null ? 0.0 : toDoublePercent(yesterdayReport.getAccuracy());
    double delta = Math.round((accuracy - yesterdayAccuracy) * 10.0) / 10.0;

    return new HomeQuickStats(solved, minutes, Math.round(accuracy * 10.0) / 10.0, xp, delta);
  }

  public HomeQuickMenu quickMenu() {
    List<HomeQuickMenuItem> items = List.of(
        new HomeQuickMenuItem("메인 학습", "/study/main", "book"),
        new HomeQuickMenuItem("보조 학습", "/study/assist", "sparkles"),
        new HomeQuickMenuItem("대전/이벤트", "/versus", "trophy"),
        new HomeQuickMenuItem("커뮤니티", "/community", "chat")
    );
    return new HomeQuickMenu(items);
  }

  private double toDoublePercent(BigDecimal accuracy) {
    return accuracy == null ? 0.0 : accuracy.doubleValue();
  }

  private UserXpWallet defaultWallet(String userId) {
    return UserXpWallet.builder()
        .userId(userId)
        .xpTotal(0)
        .level(1)
        .build();
  }
}

