package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.AnswerLog;
import com.OhRyue.certpilot.study.dto.ReportPlusDtos.*;
import com.OhRyue.certpilot.study.repository.AnswerLogRepository;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
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

  private final AnswerLogRepository answerRepo;
  private final QuestionTagRepository qtagRepo;

  public TagAbilityResp tagAbility(String userId, String mode) {
    // mode 필터는 현재 AnswerLog.mode 문자열로 가정 (없으면 전체)
    List<AnswerLog> logs = answerRepo.findByUserId(userId);
    if (mode != null && !mode.isBlank()) {
      logs = logs.stream().filter(l -> mode.equalsIgnoreCase(l.getMode())).toList();
    }
    // questionId → tags 캐시
    Map<Long, List<String>> tagsByQ = new HashMap<>();
    logs.stream().map(AnswerLog::getQuestionId).distinct().forEach(qid ->
        tagsByQ.put(qid, qtagRepo.findTagsByQuestionId(qid))
    );
    Map<String, int[]> agg = new HashMap<>();
    for (var l : logs) {
      var tags = tagsByQ.getOrDefault(l.getQuestionId(), List.of());
      for (String t : tags) {
        var a = agg.computeIfAbsent(t, k -> new int[2]);
        if (Boolean.TRUE.equals(l.getCorrect())) a[0] += 1;
        a[1] += 1;
      }
    }
    var items = agg.entrySet().stream().map(e -> {
      int corr = e.getValue()[0], tot = e.getValue()[1];
      double acc = tot == 0 ? 0.0 : (double) corr / tot * 100.0;
      return new TagAbilityItem(e.getKey(), corr, tot, round2(acc));
    }).sorted(Comparator.comparingDouble(TagAbilityItem::accuracy).reversed()).toList();
    return new TagAbilityResp(items);
  }

  public TimeSeriesResp timeseries(String userId, String range) {
    // range: WEEK(7일) | MONTH(30일), 기본 WEEK
    int days = "MONTH".equalsIgnoreCase(range) ? 30 : 7;
    LocalDate today = LocalDate.now(ZONE);
    LocalDate from = today.minusDays(days - 1);
    var fromTs = from.atStartOfDay(ZONE).toInstant();

    List<AnswerLog> recent = answerRepo.findByUserIdAndAnsweredAtAfter(userId, fromTs);

    Map<LocalDate, List<AnswerLog>> byDate = recent.stream()
        .collect(Collectors.groupingBy(a -> a.getAnsweredAt().atZone(ZONE).toLocalDate()));

    List<TSPoint> pts = new ArrayList<>();
    for (int i=0;i<days;i++) {
      LocalDate d = today.minusDays(days - 1 - i);
      var ls = byDate.getOrDefault(d, List.of());
      int total = ls.size();
      int correct = (int) ls.stream().filter(x -> Boolean.TRUE.equals(x.getCorrect())).count();
      double acc = total == 0 ? 0.0 : (double) correct / total * 100.0;
      pts.add(new TSPoint(d, correct, total, round2(acc)));
    }
    return new TimeSeriesResp(pts);
  }

  private static double round2(double v) { return Math.round(v * 100.0) / 100.0; }
}
