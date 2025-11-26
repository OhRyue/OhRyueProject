package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.UserPointWallet;
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
import com.OhRyue.certpilot.progress.repository.UserPointWalletRepository;
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
  private final UserPointWalletRepository userPointWalletRepository;

  public HomeOverview overview(String userId) {
    try {
      // JWT 기반 Feign Interceptor가 Authorization 헤더를 자동으로 붙인다고 가정
      AccountMeResponse me = accountClient.me();
      AccountMeResponse.Profile profile = me != null ? me.profile() : null;
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
      if (me != null && me.goal() != null) {
        goal = new HomeGoal(
            me.goal().certId(),
            me.goal().targetExamMode(),
            me.goal().targetRoundId(),
            me.goal().ddayCached()
        );
      }

      return new HomeOverview(userCard, goal);
    } catch (Exception e) {
      // Feign 호출 실패 시 기본값 반환
      UserXpWallet wallet = xpService.getWallet(userId);
      UserStreak streak = streakService.get(userId);
      HomeUserCard userCard = new HomeUserCard(
          userId,
          null,
          null,
          wallet.getLevel(),
          wallet.getXpTotal(),
          streak.getCurrentDays()
      );
      return new HomeOverview(userCard, null);
    }
  }

  public HomeProgressCard progressCard(String userId) {
    try {
      AccountMeResponse me = accountClient.me();
      if (me == null || me.goal() == null || me.goal().certId() == null) {
        return new HomeProgressCard(0, 0, 0, 0.0, null);
      }
      // StudyReportClient 쪽은 JWT 기반으로 현재 사용자 기준 집계한다고 가정
      return studyReportClient.progressCard(me.goal().certId());
    } catch (Exception e) {
      // Feign 호출 실패 시 기본값 반환
      return new HomeProgressCard(0, 0, 0, 0.0, null);
    }
  }

  public HomeRanking ranking(String userId) {
    try {
      List<UserRankScore> top = rankService.topN(5);
      List<String> userIds = top.stream()
          .map(UserRankScore::getUserId)
          .collect(Collectors.toList());

      if (!userIds.contains(userId)) {
        userIds.add(userId);
      }

      List<ProfileSummaryResponse> summaries = List.of();
      try {
        summaries = userIds.isEmpty()
            ? List.of()
            : accountClient.summaries(userIds);
      } catch (Exception e) {
        // Feign 호출 실패 시 빈 리스트로 처리
      }

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
    } catch (Exception e) {
      // 전체 실패 시 빈 랭킹 반환
      return new HomeRanking(List.of(), null, Instant.now().toString());
    }
  }

  public HomeQuickStats quickStats(String userId) {
    LocalDate today = LocalDate.now(KST);
    LocalDate yesterday = today.minusDays(1);

    ReportDaily todayReport = reportDailyRepository
        .findByUserIdAndDate(userId, today)
        .orElse(null);
    ReportDaily yesterdayReport = reportDailyRepository
        .findByUserIdAndDate(userId, yesterday)
        .orElse(null);

    int solved = todayReport == null ? 0 : todayReport.getSolvedCount();
    int minutes = todayReport == null ? 0 : todayReport.getTimeSpentSec() / 60;
    double accuracy = todayReport == null ? 0.0 : toDoublePercent(todayReport.getAccuracy());
    int xp = todayReport == null ? 0 : todayReport.getXpGained();

    double yesterdayAccuracy = yesterdayReport == null ? 0.0 : toDoublePercent(yesterdayReport.getAccuracy());
    double deltaRaw = accuracy - yesterdayAccuracy;
    double delta = Math.round(deltaRaw * 10.0) / 10.0;

    UserPointWallet pointWallet = userPointWalletRepository.findById(userId).orElse(null);
    long pointBalance = pointWallet == null ? 0L : pointWallet.getPointTotal();

    return new HomeQuickStats(
        solved,
        minutes,
        Math.round(accuracy * 10.0) / 10.0,
        xp,
        delta,
        pointBalance
    );
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
