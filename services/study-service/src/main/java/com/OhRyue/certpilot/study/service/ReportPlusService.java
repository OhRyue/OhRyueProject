package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.ReportPlusDtos.*;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ReportPlusService {

  private static final ZoneId ZONE = ZoneId.of("Asia/Seoul");

  private final UserAnswerRepository userAnswerRepository;
  private final QuestionTagRepository questionTagRepository;

  public TagAbilityResp tagAbility(String userId, String mode) {
    List<UserAnswer> answers = userAnswerRepository.findByUserId(userId);

    ExamMode examMode = null;
    if (mode != null && !mode.isBlank()) {
      try {
        examMode = ExamMode.valueOf(mode.trim().toUpperCase(Locale.ROOT));
      } catch (IllegalArgumentException ignore) {
        examMode = null;
      }
    }
    final ExamMode filterMode = examMode;
    if (filterMode != null) {
      answers = answers.stream()
          .filter(ans -> ans.getExamMode() == filterMode)
          .toList();
    }

    Map<Long, List<String>> tagsByQuestion = new HashMap<>();
    answers.stream()
        .map(UserAnswer::getQuestionId)
        .distinct()
        .forEach(qid -> tagsByQuestion.put(qid, questionTagRepository.findTagsByQuestionId(qid)));

    Map<String, int[]> accumulator = new HashMap<>();
    for (UserAnswer answer : answers) {
      List<String> tags = tagsByQuestion.getOrDefault(answer.getQuestionId(), List.of());
      for (String tag : tags) {
        int[] stat = accumulator.computeIfAbsent(tag, k -> new int[2]);
        if (Boolean.TRUE.equals(answer.getCorrect())) stat[0] += 1;
        stat[1] += 1;
      }
    }

    List<TagAbilityItem> items = accumulator.entrySet().stream()
        .map(entry -> {
          int correct = entry.getValue()[0];
          int total = entry.getValue()[1];
          double accuracy = total == 0 ? 0.0 : (double) correct / total * 100.0;
          return new TagAbilityItem(entry.getKey(), correct, total, round2(accuracy));
        })
        .sorted(Comparator.comparingDouble(TagAbilityItem::accuracy).reversed())
        .toList();

    return new TagAbilityResp(items);
  }

  public TimeSeriesResp timeseries(String userId, String range) {
    int days = "MONTH".equalsIgnoreCase(range) ? 30 : 7;
    LocalDate today = LocalDate.now(ZONE);
    LocalDate from = today.minusDays(days - 1);
    Instant fromTs = from.atStartOfDay(ZONE).toInstant();

    List<UserAnswer> recent = userAnswerRepository.findByUserIdAndAnsweredAtAfter(userId, fromTs);
    Map<LocalDate, List<UserAnswer>> byDate = recent.stream()
        .collect(Collectors.groupingBy(ans -> ans.getAnsweredAt().atZone(ZONE).toLocalDate()));

    List<TSPoint> points = new ArrayList<>();
    for (int i = 0; i < days; i++) {
      LocalDate date = today.minusDays(days - 1 - i);
      List<UserAnswer> dayAnswers = byDate.getOrDefault(date, List.of());
      int total = dayAnswers.size();
      int correct = (int) dayAnswers.stream().filter(ans -> Boolean.TRUE.equals(ans.getCorrect())).count();
      double accuracy = total == 0 ? 0.0 : (double) correct / total * 100.0;
      points.add(new TSPoint(date, correct, total, round2(accuracy)));
    }

    return new TimeSeriesResp(points);
  }

  private static double round2(double v) {
    return Math.round(v * 100.0) / 100.0;
  }
}
