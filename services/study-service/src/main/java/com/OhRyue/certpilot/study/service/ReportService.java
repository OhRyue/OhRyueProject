package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.dto.ReportDtos.*;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.OhRyue.certpilot.study.repository.TopicRepository;
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

  private final UserAnswerRepository userAnswerRepository;
  private final UserProgressRepository userProgressRepository;
  private final TopicRepository topicRepository;

  public ReportSummaryResp summary(String userId) {
    List<UserAnswer> allAnswers = userAnswerRepository.findByUserId(userId);

    long totalSolved = allAnswers.size();
    double avgAccuracy = percent(accuracy(allAnswers));

    LocalDate today = LocalDate.now(ZONE);
    Instant last7Start = today.minusDays(6).atStartOfDay(ZONE).toInstant();
    Instant prev7Start = today.minusDays(13).atStartOfDay(ZONE).toInstant();
    Instant prev7EndExclusive = today.minusDays(6).atStartOfDay(ZONE).toInstant();

    List<UserAnswer> last7 = allAnswers.stream()
        .filter(ans -> !ans.getAnsweredAt().isBefore(last7Start))
        .toList();
    List<UserAnswer> prev7 = allAnswers.stream()
        .filter(ans -> !ans.getAnsweredAt().isBefore(prev7Start) && ans.getAnsweredAt().isBefore(prev7EndExclusive))
        .toList();

    long weeklySolved = last7.size();
    double last7Acc = percent(accuracy(last7));
    double prev7Acc = percent(accuracy(prev7));
    double delta = round2(last7Acc - prev7Acc);

    int streak = calcStreakDays(allAnswers);

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
    List<UserAnswer> recent = userAnswerRepository.findByUserIdAndAnsweredAtAfter(userId, fromTs);

    Map<LocalDate, List<UserAnswer>> byDate = recent.stream().collect(Collectors.groupingBy(
        ans -> ans.getAnsweredAt().atZone(ZONE).toLocalDate()
    ));

    List<LocalDate> dates = new ArrayList<>();
    for (int i = 0; i < days; i++) {
      dates.add(today.minusDays(i));
    }

    List<RecentDailyItem> items = new ArrayList<>();
    for (LocalDate date : dates) {
      List<UserAnswer> dayAnswers = byDate.getOrDefault(date, List.of());
      int total = dayAnswers.size();
      int correct = (int) dayAnswers.stream().filter(ans -> Boolean.TRUE.equals(ans.getCorrect())).count();
      double acc = percent(total == 0 ? 0.0 : (double) correct / total);
      items.add(new RecentDailyItem(date, correct, total, round2(acc)));
    }
    return new RecentResultsResp(items);
  }

  public ProgressCardResp progressCard(String userId, Long certId) {
    if (certId == null) {
      return new ProgressCardResp(0, 0, 0, 0.0, null);
    }

    List<Topic> topics = topicRepository.findByCertId(certId);
    int totalTopics = topics.size();
    if (totalTopics == 0) {
      return new ProgressCardResp(0, 0, 0, 0.0, null);
    }

    Set<Long> topicIds = topics.stream()
        .map(Topic::getId)
        .collect(Collectors.toSet());

    List<UserProgress> progresses = userProgressRepository.findByUserId(userId);
    long completed = progresses.stream()
        .filter(progress -> topicIds.contains(progress.getTopicId()))
        .filter(progress ->
            Optional.ofNullable(progress.getWrittenDoneCnt()).orElse(0) > 0 ||
            Optional.ofNullable(progress.getPracticalDoneCnt()).orElse(0) > 0)
        .count();

    long pending = Math.max(0, totalTopics - completed);
    double completionRate = Math.round(((double) completed / totalTopics) * 1000.0) / 10.0;

    String lastStudiedAt = progresses.stream()
        .filter(progress -> topicIds.contains(progress.getTopicId()))
        .map(UserProgress::getLastStudiedAt)
        .filter(Objects::nonNull)
        .max(Comparator.naturalOrder())
        .map(instant -> instant.atZone(ZONE).toOffsetDateTime().toString())
        .orElse(null);

    return new ProgressCardResp(
        totalTopics,
        (int) completed,
        (int) pending,
        completionRate,
        lastStudiedAt
    );
  }

  private static double accuracy(List<UserAnswer> answers) {
    if (answers.isEmpty()) return 0.0;
    long correct = answers.stream().filter(ans -> Boolean.TRUE.equals(ans.getCorrect())).count();
    return (double) correct / answers.size();
  }

  private static double percent(double v) {
    return v * 100.0;
  }

  private static double round2(double v) {
    return Math.round(v * 100.0) / 100.0;
  }

  private int calcStreakDays(List<UserAnswer> all) {
    if (all.isEmpty()) return 0;
    Set<LocalDate> days = all.stream()
        .map(ans -> ans.getAnsweredAt().atZone(ZONE).toLocalDate())
        .collect(Collectors.toSet());

    int streak = 0;
    LocalDate cursor = LocalDate.now(ZONE);
    while (days.contains(cursor)) {
      streak++;
      cursor = cursor.minusDays(1);
    }
    return streak;
  }
}
