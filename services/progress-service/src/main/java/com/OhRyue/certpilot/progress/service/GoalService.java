package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.AssistGoalDaily;
import com.OhRyue.certpilot.progress.domain.AssistGoalDailyKey;
import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.dto.GoalDtos;
import com.OhRyue.certpilot.progress.repository.AssistGoalDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.List;

@Service
@RequiredArgsConstructor
public class GoalService {

  private static final ZoneId KST = ZoneId.of("Asia/Seoul");

  private final AssistGoalDailyRepository goalRepository;
  private final ReportDailyRepository reportDailyRepository;

  @Transactional(readOnly = true)
  public AssistGoalDaily getToday(String userId) {
    LocalDate today = LocalDate.now(KST);
    return goalRepository.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId)
            .date(today)
            .targetCount(0)
            .progressCount(0)
            .build());
  }

  @Transactional
  public AssistGoalDaily setTarget(String userId, int target) {
    LocalDate today = LocalDate.now(KST);
    AssistGoalDaily goal = goalRepository.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId)
            .date(today)
            .targetCount(0)
            .progressCount(0)
            .build());
    goal.setTargetCount(Math.max(0, target));
    return goalRepository.save(goal);
  }

  @Transactional
  public AssistGoalDaily increment(String userId, int increment) {
    LocalDate today = LocalDate.now(KST);
    AssistGoalDaily goal = goalRepository.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId)
            .date(today)
            .targetCount(0)
            .progressCount(0)
            .build());
    int next = Math.max(0, goal.getProgressCount() + increment);
    int capped = goal.getTargetCount() > 0 ? Math.min(next, goal.getTargetCount()) : next;
    goal.setProgressCount(capped);
    return goalRepository.save(goal);
  }

  @Transactional(readOnly = true)
  public GoalDtos.AssistSummary summary(String userId) {
    LocalDate today = LocalDate.now(KST);
    AssistGoalDaily todayGoal = getToday(userId);

    GoalDtos.DailyGoalStatus dailyStatus = toDailyStatus(todayGoal);

    WeekFields weekFields = WeekFields.ISO;
    LocalDate startOfWeek = today.with(weekFields.dayOfWeek(), 1);
    LocalDate endOfWeek = startOfWeek.plusDays(6);

    List<ReportDaily> weeklyReports = reportDailyRepository.findByUserIdAndDateBetween(
        userId, startOfWeek, endOfWeek);
    List<AssistGoalDaily> weeklyGoals = goalRepository.findByUserIdAndDateBetween(
        userId, startOfWeek, endOfWeek);

    GoalDtos.WeeklyStats weeklyStats = toWeeklyStats(today, weeklyReports, weeklyGoals);

    return new GoalDtos.AssistSummary(dailyStatus, weeklyStats);
  }

  private GoalDtos.DailyGoalStatus toDailyStatus(AssistGoalDaily entity) {
    int target = Math.max(0, entity.getTargetCount());
    int progress = Math.max(0, entity.getProgressCount());
    int remaining = target == 0 ? 0 : Math.max(0, target - progress);
    boolean completed = target > 0 && progress >= target;
    Instant updatedAt = entity.getUpdatedAt() == null ? Instant.now() : entity.getUpdatedAt();
    return new GoalDtos.DailyGoalStatus(entity.getDate(), target, progress, remaining, completed, updatedAt);
  }

  private GoalDtos.WeeklyStats toWeeklyStats(LocalDate today,
                                             List<ReportDaily> reports,
                                             List<AssistGoalDaily> goals) {
    WeekFields weekFields = WeekFields.ISO;
    int week = today.get(weekFields.weekOfWeekBasedYear());
    int year = today.get(weekFields.weekBasedYear());
    String weekIso = "%d-W%02d".formatted(year, week);

    int solved = reports.stream().mapToInt(ReportDaily::getSolvedCount).sum();
    int correct = reports.stream().mapToInt(ReportDaily::getCorrectCount).sum();
    double accuracy = solved == 0 ? 0.0 : Math.round((correct * 1000.0 / solved)) / 10.0;

    int goalCompletedDays = (int) goals.stream()
        .filter(g -> g.getTargetCount() > 0 && g.getProgressCount() >= g.getTargetCount())
        .count();
    int totalTrackedDays = (int) goals.stream()
        .filter(g -> g.getTargetCount() > 0)
        .count();

    return new GoalDtos.WeeklyStats(weekIso, solved, correct, accuracy, goalCompletedDays, totalTrackedDays);
  }
}
