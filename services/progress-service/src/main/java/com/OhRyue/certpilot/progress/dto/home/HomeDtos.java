package com.OhRyue.certpilot.progress.dto.home;

import io.swagger.v3.oas.annotations.media.Schema;

import java.time.LocalDate;
import java.util.List;

public final class HomeDtos {

  private HomeDtos() {}

  @Schema(description = "홈 화면 사용자 카드")
  public record HomeUserCard(
      @Schema(description = "사용자 ID") String userId,
      @Schema(description = "닉네임") String nickname,
      @Schema(description = "스킨 ID") Long skinId,
      @Schema(description = "현재 레벨") int level,
      @Schema(description = "누적 경험치") long xpTotal,
      @Schema(description = "연속 학습 일수") int streakDays
  ) {}

  @Schema(description = "목표 자격증 정보")
  public record HomeGoal(
      @Schema(description = "자격증 ID") Long certId,
      @Schema(description = "목표 모드") String targetExamMode,
      @Schema(description = "목표 회차 ID") Long targetRoundId,
      @Schema(description = "목표 시험 날짜") LocalDate targetExamDate,
      @Schema(description = "D-Day 캐시 값") Integer dday
  ) {}

  @Schema(description = "홈 개요 응답")
  public record HomeOverview(
      HomeUserCard user,
      HomeGoal goal
  ) {}

  @Schema(description = "학습 진행 카드")
  public record HomeProgressCard(
      @Schema(description = "총 토픽 수") int totalTopics,
      @Schema(description = "완료된 토픽 수") int completedTopics,
      @Schema(description = "미완료 토픽 수") int pendingTopics,
      @Schema(description = "달성률(%)") double completionRate,
      @Schema(description = "마지막 학습 시각") String lastStudiedAt
  ) {}

  @Schema(description = "랭킹 카드 항목")
  public record HomeRankingItem(
      @Schema(description = "사용자 ID") String userId,
      @Schema(description = "닉네임") String nickname,
      @Schema(description = "스킨 ID") Long skinId,
      @Schema(description = "레벨") int level,
      @Schema(description = "점수") long score,
      @Schema(description = "누적 경험치") long xpTotal,
      @Schema(description = "현재 사용자 여부") boolean self,
      @Schema(description = "순위") int rank
  ) {}

  @Schema(description = "랭킹 카드")
  public record HomeRanking(
      @Schema(description = "상위 5명") List<HomeRankingItem> top5,
      @Schema(description = "내 순위 정보") HomeRankingItem me,
      @Schema(description = "생성 시각") String generatedAt
  ) {}

  @Schema(description = "오늘의 학습 실적")
  public record HomeQuickStats(
      @Schema(description = "오늘 푼 문제 수") int solvedToday,
      @Schema(description = "오늘 학습 시간(분)") int minutesToday,
      @Schema(description = "오늘 평균 정답률(%)") double accuracyToday,
      @Schema(description = "오늘 획득 XP") int xpToday,
      @Schema(description = "정답률 전일 대비 증감(%)") double accuracyDelta,
      @Schema(description = "현재 보유 포인트") long pointBalance
  ) {}

  @Schema(description = "홈 빠른 메뉴 항목")
  public record HomeQuickMenuItem(
      @Schema(description = "메뉴 이름") String label,
      @Schema(description = "연결 경로") String path,
      @Schema(description = "아이콘 키") String icon
  ) {}

  @Schema(description = "홈 빠른 메뉴")
  public record HomeQuickMenu(
      @Schema(description = "빠른 메뉴 항목들") List<HomeQuickMenuItem> items
  ) {}
}
