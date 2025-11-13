package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.temporal.WeekFields;

@Component
@RequiredArgsConstructor
@Slf4j
public class LeaderboardScheduler {

  private final LeaderboardService leaderboardService;

  @Scheduled(cron = "0 10 3 * * *")
  public void refreshDailyLeaderboards() {
    LocalDate today = LocalDate.now();
    leaderboardService.recompute(RankScope.OVERALL, today.toString());
    leaderboardService.recompute(RankScope.HALL_OF_FAME, today.toString());
    log.debug("Daily leaderboards refreshed for {}", today);
  }

  @Scheduled(cron = "0 20 3 * * MON")
  public void refreshWeeklyLeaderboard() {
    LocalDate now = LocalDate.now();
    WeekFields fields = WeekFields.ISO;
    int week = now.get(fields.weekOfWeekBasedYear());
    int year = now.get(fields.weekBasedYear());
    String weekKey = "%d-W%02d".formatted(year, week);
    leaderboardService.recompute(RankScope.WEEKLY, weekKey);
    log.debug("Weekly leaderboard refreshed for {}", weekKey);
  }
}

