package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.AnswerLog;
import com.OhRyue.certpilot.study.dto.ReportDtos.*;
import com.OhRyue.certpilot.study.repository.AnswerLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ReportService {

  private static final ZoneId ZONE = ZoneId.of("Asia/Seoul");

  private final AnswerLogRepository answerRepo;

  public ReportSummaryResp summary(String userId) {
    // 전체 로그
    List<AnswerLog> all = answerRepo.findByUserId(userId);

    long totalSolved = all.size();
    double avgAccuracy = percent(accuracy(all));

    // 최근/직전 7일 구간
    LocalDate today = LocalDate.now(ZONE);
    Instant last7Start = today.minusDays(6).atStartOfDay(ZONE).toInstant(); // 오늘 포함 7일
    Instant prev7Start = today.minusDays(13).atStartOfDay(ZONE).toInstant();
    Instant prev7EndExclusive = today.minusDays(6).atStartOfDay(ZONE).toInstant();

    List<AnswerLog> last7 = all.stream().filter(a -> !a.getAnsweredAt().isBefore(last7Start)).toList();
    List<AnswerLog> prev7 = all.stream().filter(a ->
        !a.getAnsweredAt().isBefore(prev7Start) && a.getAnsweredAt().isBefore(prev7EndExclusive)
    ).toList();

    long weeklySolved = last7.size();
    double last7Acc = percent(accuracy(last7));
    double prev7Acc = percent(accuracy(prev7));
    double delta = round2(last7Acc - prev7Acc);

    int streak = calcStreakDays(all);

    return new ReportSummaryResp(
        totalSolved,
        weeklySolved,
        round2(avgAccuracy),
        round2(last7Acc),
        round2(prev7Acc),
        delta,
        streak
    );
  }

  public RecentResultsResp recentDaily(String userId, int days) {
    if (days <= 0) days = 14;
    LocalDate today = LocalDate.now(ZONE);
    LocalDate from = today.minusDays(days - 1);

    Instant fromTs = from.atStartOfDay(ZONE).toInstant();

    List<AnswerLog> recent = answerRepo.findByUserIdAndAnsweredAtAfter(userId, fromTs);

    // 일자별 집계
    Map<LocalDate, List<AnswerLog>> byDate = recent.stream().collect(Collectors.groupingBy(
        l -> l.getAnsweredAt().atZone(ZONE).toLocalDate()
    ));

    // 최신 날짜부터 정렬
    List<LocalDate> dates = new ArrayList<>();
    for (int i = 0; i < days; i++) dates.add(today.minusDays(i));

    List<RecentDailyItem> items = new ArrayList<>();
    for (LocalDate d : dates) {
      List<AnswerLog> dayLogs = byDate.getOrDefault(d, List.of());
      int total = dayLogs.size();
      int correct = (int) dayLogs.stream().filter(a -> Boolean.TRUE.equals(a.getCorrect())).count();
      double acc = percent(total == 0 ? 0.0 : (double) correct / total);
      items.add(new RecentDailyItem(d, correct, total, round2(acc)));
    }
    return new RecentResultsResp(items);
  }

  /* ============ 내부 유틸 ============ */

  private static double accuracy(List<AnswerLog> logs) {
    if (logs.isEmpty()) return 0.0;
    long corr = logs.stream().filter(a -> Boolean.TRUE.equals(a.getCorrect())).count();
    return (double) corr / logs.size();
  }

  private static double percent(double v) { return v * 100.0; }

  private static double round2(double v) { return Math.round(v * 100.0) / 100.0; }

  /** 연속 학습일: 오늘부터 거꾸로, 매일 최소 1건 이상 풀이가 있으면 +1 */
  private int calcStreakDays(List<AnswerLog> all) {
    if (all.isEmpty()) return 0;
    Set<LocalDate> days = all.stream()
        .map(a -> a.getAnsweredAt().atZone(ZONE).toLocalDate())
        .collect(Collectors.toSet());

    int streak = 0;
    LocalDate cur = LocalDate.now(ZONE);
    while (days.contains(cur)) {
      streak++;
      cur = cur.minusDays(1);
    }
    return streak;
  }
}
