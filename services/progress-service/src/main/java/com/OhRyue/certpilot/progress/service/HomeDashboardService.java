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
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
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
  private final ObjectMapper objectMapper;

  public HomeOverview overview(String userId) {
    try {
      // JWT 기반 Feign Interceptor가 Authorization 헤더를 자동으로 붙인다고 가정
      AccountMeResponse me = accountClient.me();
      
      // 온보딩 미완료 체크
      if (me != null && me.onboarding() != null && !me.onboarding().completed()) {
        throw new com.OhRyue.certpilot.progress.exception.OnboardingRequiredException();
      }
      
      AccountMeResponse.Profile profile = me != null ? me.profile() : null;
      UserXpWallet wallet = xpService.getWallet(userId);
      UserStreak streak = streakService.get(userId);

      HomeUserCard userCard = new HomeUserCard(
          userId,
          profile == null ? null : profile.nickname(),
          profile == null ? null : profile.skinId(),
          wallet.getLevel(),
          wallet.getXpTotal(),
          streak.getCurrentDays()
      );

      HomeGoal goal = null;
      if (me != null && me.goal() != null) {
        Integer dday = me.goal().ddayCached();
        // ddayCached가 null이고 targetExamDate가 있으면 계산
        if (dday == null && me.goal().targetExamDate() != null) {
          LocalDate today = LocalDate.now(KST);
          dday = (int) ChronoUnit.DAYS.between(today, me.goal().targetExamDate());
        }
        goal = new HomeGoal(
            me.goal().certId(),
            me.goal().targetExamMode(),
            me.goal().targetRoundId(),
            me.goal().targetExamDate(),
            dday
        );
      }

      return new HomeOverview(userCard, goal);
    } catch (com.OhRyue.certpilot.progress.exception.OnboardingRequiredException e) {
      // 온보딩 미완료 예외는 그대로 전파
      throw e;
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

  public HomeProgressCard progressCard(String userId, String mode) {
    try {
      AccountMeResponse me = accountClient.me();
      
      // 온보딩 미완료 체크
      if (me != null && me.onboarding() != null && !me.onboarding().completed()) {
        throw new com.OhRyue.certpilot.progress.exception.OnboardingRequiredException();
      }
      
      if (me == null || me.goal() == null || me.goal().certId() == null) {
        return new HomeProgressCard(0, 0, 0, 0.0, null);
      }
      
      // mode 파라미터 검증 및 정규화
      String normalizedMode = mode != null ? mode.trim().toUpperCase() : "WRITTEN";
      if (!normalizedMode.equals("WRITTEN") && !normalizedMode.equals("PRACTICAL")) {
        // 잘못된 mode 값인 경우 기본값 반환
        return new HomeProgressCard(0, 0, 0, 0.0, null);
      }
      
      // StudyReportClient 쪽은 JWT 기반으로 현재 사용자 기준 집계한다고 가정
      // study-service가 실제 응답 구조를 확인해야 함
      // 가능성 1: 단일 모드 응답 {"totalTopics":13,...} -> Feign이 자동 매핑
      // 가능성 2: 복합 응답 {"written":{...},"practical":{...}} -> 수동 파싱 필요
      try {
        String rawResponse = studyReportClient.progressCardRaw(me.goal().certId(), normalizedMode);
        
        if (rawResponse == null || rawResponse.isBlank()) {
          return new HomeProgressCard(0, 0, 0, 0.0, null);
        }
        
        // JSON 파싱 시도
        Map<String, Object> responseMap = objectMapper.readValue(rawResponse, Map.class);
        
        // 복합 응답인지 확인: {"written":{...},"practical":{...}} 형태
        String modeKey = normalizedMode.toLowerCase(); // "WRITTEN" -> "written"
        if (responseMap.containsKey("written") || responseMap.containsKey("practical")) {
          // 복합 응답: 요청한 모드만 추출
          if (!responseMap.containsKey(modeKey)) {
            return new HomeProgressCard(0, 0, 0, 0.0, null);
          }
          
          @SuppressWarnings("unchecked")
          Map<String, Object> modeData = (Map<String, Object>) responseMap.get(modeKey);
          
          // modeData에서 HomeProgressCard로 변환
          int totalTopics = modeData.get("totalTopics") != null ? 
              ((Number) modeData.get("totalTopics")).intValue() : 0;
          int completedTopics = modeData.get("completedTopics") != null ? 
              ((Number) modeData.get("completedTopics")).intValue() : 0;
          int pendingTopics = modeData.get("pendingTopics") != null ? 
              ((Number) modeData.get("pendingTopics")).intValue() : 0;
          double completionRate = modeData.get("completionRate") != null ? 
              ((Number) modeData.get("completionRate")).doubleValue() : 0.0;
          String lastStudiedAt = modeData.get("lastStudiedAt") != null ? 
              modeData.get("lastStudiedAt").toString() : null;
          
          return new HomeProgressCard(totalTopics, completedTopics, pendingTopics, completionRate, lastStudiedAt);
        } else {
          // 단일 응답: {"totalTopics":13,...} 형태 - 직접 파싱
          int totalTopics = responseMap.get("totalTopics") != null ? 
              ((Number) responseMap.get("totalTopics")).intValue() : 0;
          int completedTopics = responseMap.get("completedTopics") != null ? 
              ((Number) responseMap.get("completedTopics")).intValue() : 0;
          int pendingTopics = responseMap.get("pendingTopics") != null ? 
              ((Number) responseMap.get("pendingTopics")).intValue() : 0;
          double completionRate = responseMap.get("completionRate") != null ? 
              ((Number) responseMap.get("completionRate")).doubleValue() : 0.0;
          String lastStudiedAt = responseMap.get("lastStudiedAt") != null ? 
              responseMap.get("lastStudiedAt").toString() : null;
          
          return new HomeProgressCard(totalTopics, completedTopics, pendingTopics, completionRate, lastStudiedAt);
        }
      } catch (Exception feignException) {
        // 예외 발생 시 로깅하고 기본값 반환
        System.err.println("[HomeDashboardService.progressCard] 파싱 실패: " + feignException.getMessage());
        feignException.printStackTrace();
        return new HomeProgressCard(0, 0, 0, 0.0, null);
      }
    } catch (com.OhRyue.certpilot.progress.exception.OnboardingRequiredException e) {
      // 온보딩 미완료 예외는 그대로 전파
      throw e;
    } catch (Exception e) {
      // Feign 호출 실패 시 기본값 반환
      return new HomeProgressCard(0, 0, 0, 0.0, null);
    }
  }

  public HomeRanking ranking(String userId) {
    try {
      // 온보딩 미완료 체크
      AccountMeResponse me = accountClient.me();
      if (me != null && me.onboarding() != null && !me.onboarding().completed()) {
        throw new com.OhRyue.certpilot.progress.exception.OnboardingRequiredException();
      }
      
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
            summary == null ? null : summary.skinId(),
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
            summary == null ? null : summary.skinId(),
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
    } catch (com.OhRyue.certpilot.progress.exception.OnboardingRequiredException e) {
      // 온보딩 미완료 예외는 그대로 전파
      throw e;
    } catch (Exception e) {
      // 전체 실패 시 빈 랭킹 반환
      return new HomeRanking(List.of(), null, Instant.now().toString());
    }
  }

  public HomeQuickStats quickStats(String userId) {
    // 온보딩 미완료 체크
    try {
      AccountMeResponse me = accountClient.me();
      if (me != null && me.onboarding() != null && !me.onboarding().completed()) {
        throw new com.OhRyue.certpilot.progress.exception.OnboardingRequiredException();
      }
    } catch (com.OhRyue.certpilot.progress.exception.OnboardingRequiredException e) {
      // 온보딩 미완료 예외는 그대로 전파
      throw e;
    } catch (Exception e) {
      // Feign 호출 실패 시 온보딩 체크는 건너뛰고 계속 진행
    }
    
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
