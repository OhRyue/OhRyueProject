package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Stream;

/**
 * 보조학습 - 실기(Short/Long) 전용 서비스
 * - 카테고리/난이도/약점 보완 세트 생성
 * - 제출 시 AIExplanationService 를 이용해 단순 채점
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistPracticalService {

  private static final List<Integer> ALLOWED_COUNTS = List.of(5, 10, 20);
  private final QuestionRepository questionRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;
  private final TopicTreeService topicTreeService;
  private final AIExplanationService aiExplanationService;

  /* ================= 카테고리: rootTopicId 기준 하위 토픽 전체 ================= */

  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(Long rootTopicId,
                                                                   Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    Set<Long> topicIds = topicTreeService.descendantsOf(rootTopicId);
    if (topicIds.isEmpty()) {
      topicIds = Set.of(rootTopicId);
    }

    List<Question> pool = new ArrayList<>();
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.SHORT));
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.LONG));

    pool = pool.stream()
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/practical/category] rootTopicId={}, topicIds={}, poolSize={}, count={}",
        rootTopicId, topicIds, pool.size(), want);

    AssistDtos.QuizSet set = pickPractical(pool, want);
    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_CATEGORY", userId, set);
  }

  /* ================= 난이도 ================= */

  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(Difficulty diff,
                                                                     Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    Difficulty difficulty = (diff == null ? Difficulty.NORMAL : diff);
    int want = sanitizeCount(count);

    List<Question> pool = Stream.concat(
            questionRepository.findByModeAndTypeAndDifficulty(
                ExamMode.PRACTICAL, QuestionType.SHORT, difficulty).stream(),
            questionRepository.findByModeAndTypeAndDifficulty(
                ExamMode.PRACTICAL, QuestionType.LONG, difficulty).stream()
        )
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/practical/difficulty] diff={}, poolSize={}, count={}",
        difficulty, pool.size(), want);

    AssistDtos.QuizSet set = pickPractical(pool, want);
    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_DIFFICULTY", userId, set);
  }

  /* ================= 약점 보완 ================= */

  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    List<UserProgress> ps = progressRepository.findByUserId(userId);
    ps.sort(Comparator.comparingDouble(this::avgScore)
        .thenComparing(UserProgress::getUpdatedAt));
    List<Long> targetTopics = ps.stream()
        .map(UserProgress::getTopicId)
        .limit(5)
        .toList();

    List<Question> pool;
    if (targetTopics.isEmpty()) {
      pool = Stream.concat(
              questionRepository.findByModeAndTypeAndDifficulty(
                  ExamMode.PRACTICAL, QuestionType.SHORT, Difficulty.NORMAL).stream(),
              questionRepository.findByModeAndTypeAndDifficulty(
                  ExamMode.PRACTICAL, QuestionType.LONG, Difficulty.NORMAL).stream()
          )
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    } else {
      pool = new ArrayList<>();
      pool.addAll(questionRepository
          .findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.SHORT));
      pool.addAll(questionRepository
          .findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.LONG));
      pool = pool.stream()
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    }

    log.debug("[assist/practical/weakness] userId={}, targets={}, poolSize={}, count={}",
        userId, targetTopics, pool.size(), want);

    AssistDtos.QuizSet set = pickPractical(pool, want);
    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_WEAKNESS", userId, set);
  }

  /* ================= 제출(실기 – SHORT/LONG, LLM 채점) ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submit(
      AssistDtos.PracticalSubmitReq req
  ) {
    String userId = AuthUserUtil.getCurrentUserId();

    if (req == null || req.answers() == null || req.answers().isEmpty()) {
      return new FlowDtos.StepEnvelope<>(
          null,
          "ASSIST_PRACTICAL",
          "ASSIST_PRACTICAL_SUBMIT",
          "COMPLETE",
          null,
          fetchStats(userId),
          new AssistDtos.PracticalSubmitResp(0, 0, List.of())
      );
    }

    List<Long> qIds = req.answers().stream()
        .map(AssistDtos.PracticalAnswer::questionId)
        .filter(Objects::nonNull)
        .toList();

    Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(java.util.stream.Collectors.toMap(Question::getId, q -> q));

    List<AssistDtos.PracticalResultItem> items = new ArrayList<>();
    int totalScore = 0;
    int graded = 0;

    for (AssistDtos.PracticalAnswer ans : req.answers()) {
      Question q = questionMap.get(ans.questionId());
      if (q == null) {
        continue;
      }

      AIExplanationService.PracticalResult result =
          aiExplanationService.explainAndScorePractical(q, ans.userText());

      int score = Optional.ofNullable(result.score()).orElse(0);
      totalScore += score;
      graded++;

      String baseExplain = Optional.ofNullable(q.getSolutionText()).orElse("");
      String aiExplain = result.explain();

      items.add(new AssistDtos.PracticalResultItem(
          q.getId(),
          score,
          baseExplain,
          aiExplain
      ));
    }

    int total = graded;
    int avgScore = (total == 0 ? 0 : (int) Math.round(totalScore * 1.0 / total));

    AssistDtos.PracticalSubmitResp payload =
        new AssistDtos.PracticalSubmitResp(total, avgScore, items);

    return new FlowDtos.StepEnvelope<>(
        null,
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_SUBMIT",
        "COMPLETE",
        null,
        fetchStats(userId),
        payload
    );
  }

  /* ================= 내부 유틸 ================= */

  private AssistDtos.QuizSet pickPractical(List<Question> pool, int count) {
    if (pool == null || pool.isEmpty()) {
      return new AssistDtos.QuizSet(List.of());
    }

    // 중복 제거 후 셔플
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);

    int lim = Math.min(copy.size(), Math.max(1, count));

    List<AssistDtos.QuizQ> items = copy.subList(0, lim).stream()
        .map(q -> new AssistDtos.QuizQ(
            q.getId(),
            Optional.ofNullable(q.getStem()).orElse(""),
            List.of(), // 실기는 선택지 없음
            q.getImageUrl()
        ))
        .toList();

    return new AssistDtos.QuizSet(items);
  }

  private double avgScore(UserProgress p) {
    return Optional.ofNullable(p.getPracticalAvgScore()).orElse(0.0);
  }

  /** 10/20/50 중 가장 가까운 값으로 보정 (미지정/이상치 방지) */
  private int sanitizeCount(Integer v) {
    if (v == null) return 20;
    if (ALLOWED_COUNTS.contains(v)) return v;
    return ALLOWED_COUNTS.stream()
        .min(Comparator.comparingInt(a -> Math.abs(a - v)))
        .orElse(20);
  }

  private FlowDtos.StepEnvelope<AssistDtos.QuizSet> wrap(String mode,
                                                         String step,
                                                         String userId,
                                                         AssistDtos.QuizSet payload) {
    Map<String, Object> meta = fetchStats(userId);
    return new FlowDtos.StepEnvelope<>(
        null,
        mode,
        step,
        "IN_PROGRESS",
        null,
        meta,
        payload
    );
  }

  private Map<String, Object> fetchStats(String userId) {
    Map<String, Object> meta = new HashMap<>();
    if (userId == null || userId.isBlank()) {
      return meta;
    }
    try {
      ProgressQueryClient.GoalToday goal = progressQueryClient.getTodayGoal(userId);
      if (goal != null) {
        meta.put("todayGoal", Map.of(
            "target", Optional.ofNullable(goal.targetCount()).orElse(0),
            "progress", Optional.ofNullable(goal.progressCount()).orElse(0)
        ));
      }
    } catch (Exception ignored) {
    }
    try {
      ProgressQueryClient.Overview overview = progressQueryClient.overview(userId, "PRACTICAL");
      if (overview != null) {
        meta.put("weeklySolved", overview.problemsThisWeek());
        meta.put("avgAccuracy", overview.avgAccuracy());
      }
    } catch (Exception ignored) {
    }
    return meta;
  }
}
