package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.config.RecommendationProperties;
import com.OhRyue.certpilot.study.domain.AnswerLog;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.RecommendationDtos.*;
import com.OhRyue.certpilot.study.repository.AnswerLogRepository;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class RecommendationService {

  private final RecommendationProperties props;
  private final AnswerLogRepository answerRepo;
  private final QuestionRepository qRepo;
  private final QuestionTagRepository qtagRepo;
  private final QuestionChoiceRepository choiceRepo;

  /* ===================== 약점 태그 Top-N ===================== */
  public WeakTagsResp weakTags(String userId, int topN, int minTried) {

    // 전체 로그 중 내 것만 필터
    List<AnswerLog> allLogs = answerRepo.findAll().stream()
        .filter(a -> userId.equals(a.getUserId()))
        .toList();

    // questionId -> tags 매핑
    Map<Long, List<String>> qidToTags = new HashMap<>();
    allLogs.stream().map(AnswerLog::getQuestionId).distinct().forEach(qid -> {
      qidToTags.put(qid, qtagRepo.findTagsByQuestionId(qid));
    });

    // 태그별 정답/전체 집계
    Map<String, int[]> tagStat = new HashMap<>(); // [correct,total]
    for (AnswerLog log : allLogs) {
      List<String> tags = qidToTags.getOrDefault(log.getQuestionId(), List.of());
      for (String tag : tags) {
        int[] arr = tagStat.computeIfAbsent(tag, k -> new int[2]);
        if (Boolean.TRUE.equals(log.getCorrect())) arr[0] += 1;
        arr[1] += 1;
      }
    }

    // 정렬 → 상위 N개
    List<WeakTag> items = tagStat.entrySet().stream()
        .filter(e -> e.getValue()[1] >= Math.max(minTried, props.getMinTriedPerTag()))
        .map(e -> {
          int correct = e.getValue()[0];
          int total   = e.getValue()[1];
          double acc  = total == 0 ? 0.0 : (double) correct / total;
          return new WeakTag(e.getKey(), correct, total, round2(acc), round2(1.0 - acc));
        })
        .sorted(Comparator.comparingDouble(WeakTag::weakness).reversed())
        .limit(topN)
        .toList();

    return new WeakTagsResp(items);
  }

  /* ===================== 태그 기반 추천 퀴즈 ===================== */
  @Transactional
  public TagQuizSet tagQuiz(TagQuizReq req) {

    // 1) 파라미터 병합
    int count = (req.count() != null) ? req.count() : props.getDefaultCount();

    Set<Difficulty> difficulties = parseDifficulties(
        (req.allowedDifficulties() == null || req.allowedDifficulties().isEmpty())
            ? Arrays.asList(props.getAllowedDifficulties().split(","))
            : req.allowedDifficulties()
    );

    double wRecent = (req.recentWrongWeight() != null) ? req.recentWrongWeight() : props.getRecentWrongWeight();
    int    recentD = (req.recentDays() != null)         ? req.recentDays()         : props.getRecentDays();
    int    minTr   = (req.minTriedPerTag() != null)     ? req.minTriedPerTag()     : props.getMinTriedPerTag();
    double tagCap  = (req.perTagQuotaRatio() != null)   ? req.perTagQuotaRatio()   : props.getPerTagQuotaRatio();
    double explore = (req.exploreRatio() != null)       ? req.exploreRatio()       : props.getExploreRatio();

    // 2) 후보 문항 수집
    List<Long> candidates = qtagRepo.findQuestionIdsByTags(req.tags());
    if (candidates.isEmpty()) return new TagQuizSet(List.of());

    // 3) 기간/난이도/타입 필터
    Instant after = Instant.now().minus(recentD, ChronoUnit.DAYS);

    List<Question> pool = qRepo.findByIdInAndDifficultyInAndTypeIn(
        candidates, difficulties, List.of(QuestionType.MCQ)
    );
    if (pool.isEmpty()) return new TagQuizSet(List.of());

    // 4) 캐시: 문제별 태그
    Map<Long, List<String>> qidTags = new HashMap<>();
    pool.forEach(q -> qidTags.put(q.getId(), qtagRepo.findTagsByQuestionId(q.getId())));

    // 5) 최근 오답 횟수
    Map<Long, Long> recentWrongCount = answerRepo
        .findByUserIdAndQuestionIdInAndAnsweredAtAfter(
            req.userId(),
            pool.stream().map(Question::getId).toList(),
            after
        )
        .stream()
        .filter(a -> !Boolean.TRUE.equals(a.getCorrect()))
        .collect(Collectors.groupingBy(AnswerLog::getQuestionId, Collectors.counting()));

    // 6) 태그 약점도(전체기간)
    List<AnswerLog> allUserLogsForPool = answerRepo.findByUserIdAndQuestionIdIn(
        req.userId(), pool.stream().map(Question::getId).toList());
    Map<String, int[]> tagStat = buildTagStat(allUserLogsForPool, qidTags);

    Map<String, Double> tagWeakness = new HashMap<>();
    for (var e : tagStat.entrySet()) {
      int corr = e.getValue()[0], tot = e.getValue()[1];
      if (tot < minTr) continue;
      double acc = (tot == 0) ? 0.0 : (double) corr / tot;
      tagWeakness.put(e.getKey(), 1.0 - acc);
    }

    // 7) 난이도 승수
    Map<Difficulty, Double> diffMul = Map.of(
        Difficulty.EASY,   props.getDifficultyWeightEasy(),
        Difficulty.NORMAL, props.getDifficultyWeightNormal(),
        Difficulty.HARD,   props.getDifficultyWeightHard()
    );

    // 8) 스코어링
    Map<Long, Double> score = new HashMap<>();
    for (Question q : pool) {
      List<String> tags = qidTags.getOrDefault(q.getId(), List.of());
      double weak = tags.stream()
          .map(tagWeakness::get)
          .filter(Objects::nonNull)
          .mapToDouble(Double::doubleValue)
          .average()
          .orElse(0.0);

      double recentCnt  = recentWrongCount.getOrDefault(q.getId(), 0L);
      double recentNorm = Math.min(1.0, recentCnt / 3.0);

      double dm = diffMul.getOrDefault(q.getDifficulty(), 1.0);
      double sc = weak * dm * (1.0 + wRecent * recentNorm);
      score.put(q.getId(), sc);
    }

    // 9) 태그별 쿼터
    int quotaPerTag = Math.max(1, (int)Math.floor(count * tagCap));
    Map<String, Integer> tagQuota = new HashMap<>();
    req.tags().forEach(t -> tagQuota.put(t, quotaPerTag));

    // 10) 우선순위 선발
    List<Question> sorted = pool.stream()
        .sorted(Comparator.comparingDouble((Question q) -> score.getOrDefault(q.getId(), 0.0)).reversed())
        .toList();

    List<Question> strong = new ArrayList<>();
    Set<Long> seen = new HashSet<>();

    for (Question q : sorted) {
      if (strong.size() >= (int)Math.round(count * (1.0 - explore))) break;
      List<String> tags = qidTags.getOrDefault(q.getId(), List.of());
      boolean anyEligible = false;
      for (String t : tags) {
        if (tagQuota.containsKey(t) && tagQuota.get(t) > 0) { anyEligible = true; break; }
      }
      if (!anyEligible) continue;
      if (seen.contains(q.getId())) continue;

      for (String t : tags) {
        if (tagQuota.containsKey(t) && tagQuota.get(t) > 0) {
          tagQuota.put(t, tagQuota.get(t) - 1);
        }
      }
      strong.add(q);
      seen.add(q.getId());
    }

    // 11) 탐색 보강
    List<Question> restPool = pool.stream()
        .filter(q -> !seen.contains(q.getId()))
        .collect(Collectors.toCollection(ArrayList::new));
    Collections.shuffle(restPool);
    int restNeed = Math.max(0, count - strong.size());
    List<Question> explorePick = restPool.stream().limit(restNeed).toList();

    List<Question> finalSet = new ArrayList<>(strong);
    finalSet.addAll(explorePick);

    // 12) DTO 변환(보기 포함)
    List<QuizQ> out = finalSet.stream().map(q -> {
      var choices = choiceRepo.findByQuestionId(q.getId()).stream()
          .map(c -> new QuizQ.Choice(c.getLabel(), c.getText()))
          .toList();
      return new QuizQ(q.getId(), q.getText(), q.getDifficulty().name(), choices);
    }).toList();

    return new TagQuizSet(out);
  }

  /* ===================== 내부 보조 ===================== */

  private Set<Difficulty> parseDifficulties(List<String> list) {
    if (list == null || list.isEmpty()) return EnumSet.allOf(Difficulty.class);
    Set<Difficulty> out = EnumSet.noneOf(Difficulty.class);
    for (String s : list) {
      try { out.add(Difficulty.valueOf(s.trim().toUpperCase(Locale.ROOT))); }
      catch (Exception ignore) {}
    }
    return out.isEmpty() ? EnumSet.allOf(Difficulty.class) : out;
  }

  private Map<String, int[]> buildTagStat(List<AnswerLog> logs, Map<Long, List<String>> qidTags) {
    Map<String, int[]> tagStat = new HashMap<>();
    for (AnswerLog log : logs) {
      List<String> tags = qidTags.getOrDefault(log.getQuestionId(), List.of());
      for (String tag : tags) {
        int[] arr = tagStat.computeIfAbsent(tag, k -> new int[2]);
        if (Boolean.TRUE.equals(log.getCorrect())) arr[0] += 1;
        arr[1] += 1;
      }
    }
    return tagStat;
  }

  private static double round2(double v) {
    return Math.round(v * 100.0) / 100.0;
  }
}
