package com.OhRyue.certpilot.study.service;

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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 보조학습 - 실기(Short/Long) 전용 서비스
 * - 카테고리/난이도/약점 보완 세트 생성
 * - 문제 수는 10/20/50만 허용(기타 값은 가장 가까운 합법 값으로 보정)
 * - 실기는 선택지(choices) 없이 본문만 제공
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistPracticalService {

  private static final List<Integer> ALLOWED_COUNTS = List.of(10, 20, 50);

  private final QuestionRepository questionRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;

  /* ================= 카테고리 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(AssistDtos.CategoryStartReq req) {
    List<Long> topicIds = Optional.ofNullable(req.topicIds()).orElse(List.of());
    if (topicIds.isEmpty()) {
      return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_CATEGORY", req.userId(),
          new AssistDtos.QuizSet(List.of()));
    }

    List<Question> pool = questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.SHORT)
        .stream()
        .collect(Collectors.toCollection(ArrayList::new));
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.LONG));
    pool = pool.stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_CATEGORY", req.userId(),
        pickPractical(pool, sanitizeCount(req.count())));
  }

  /* ================= 난이도 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(AssistDtos.DifficultyStartReq req) {
    Difficulty diff = Objects.requireNonNullElse(req.difficulty(), Difficulty.NORMAL);

    List<Question> pool = Stream.concat(
            questionRepository.findByModeAndTypeAndDifficulty(ExamMode.PRACTICAL, QuestionType.SHORT, diff).stream(),
            questionRepository.findByModeAndTypeAndDifficulty(ExamMode.PRACTICAL, QuestionType.LONG, diff).stream()
        )
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_DIFFICULTY", req.userId(),
        pickPractical(pool, sanitizeCount(req.count())));
  }

  /* ================= 약점 보완 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(AssistDtos.WeaknessStartReq req) {
    // 실기 세밀 지표가 없을 경우, MCQ 정확도 낮은 토픽을 약점으로 간주(일관 휴리스틱)
    List<UserProgress> ps = progressRepository.findByUserId(req.userId());
    ps.sort(Comparator.comparingDouble(this::avgScore).thenComparing(UserProgress::getUpdatedAt));
    List<Long> targetTopics = ps.stream().map(UserProgress::getTopicId).limit(5).toList();

    List<Question> pool;
    if (targetTopics.isEmpty()) {
      pool = Stream.concat(
              questionRepository.findByModeAndTypeAndDifficulty(ExamMode.PRACTICAL, QuestionType.SHORT, Difficulty.NORMAL).stream(),
              questionRepository.findByModeAndTypeAndDifficulty(ExamMode.PRACTICAL, QuestionType.LONG, Difficulty.NORMAL).stream()
          )
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    } else {
      pool = questionRepository.findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.SHORT);
      pool = Stream.concat(pool.stream(),
              questionRepository.findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.LONG).stream())
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    }

    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_WEAKNESS", req.userId(),
        pickPractical(pool, sanitizeCount(req.count())));
  }

  /* ================= 내부 유틸 ================= */

  private AssistDtos.QuizSet pickPractical(List<Question> pool, int count) {
    if (pool.isEmpty()) return new AssistDtos.QuizSet(List.of());

    // 중복 제거 후 셔플
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);

    int lim = Math.min(copy.size(), Math.max(1, count));

    var items = copy.stream().limit(lim)
        .map(q -> new AssistDtos.QuizQ(
            q.getId(),
            Optional.ofNullable(q.getStem()).orElse(""),
            List.of(),
            q.getImageUrl()))
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
