package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 보조학습 - 필기(MCQ) 전용 서비스
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistWrittenService {

  private static final List<Integer> ALLOWED_COUNTS = List.of(10, 20, 50);

  private final QuestionRepository questionRepository;
  private final QuestionChoiceRepository choiceRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;

  /* ================= 카테고리 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(AssistDtos.CategoryStartReq req) {
    List<Long> topicIds = Optional.ofNullable(req.topicIds()).orElse(List.of());
    int want = sanitizeCount(req.count());

    if (topicIds.isEmpty()) {
      log.debug("[assist/written/category] empty topicIds -> empty set");
      return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_CATEGORY", req.userId(),
          new AssistDtos.QuizSet(List.of()), "WRITTEN");
    }

    List<Question> pool = questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.WRITTEN, QuestionType.MCQ)
        .stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/category] topicIds={}, poolSize={}, count={}", topicIds, pool.size(), want);
    return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_CATEGORY", req.userId(), pickMcq(pool, want), "WRITTEN");
  }

  /* ================= 난이도 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(AssistDtos.DifficultyStartReq req) {
    Difficulty diff = Objects.requireNonNullElse(req.difficulty(), Difficulty.NORMAL);
    int want = sanitizeCount(req.count());

    List<Question> pool = questionRepository
        .findByModeAndTypeAndDifficulty(ExamMode.WRITTEN, QuestionType.MCQ, diff)
        .stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/difficulty] diff={}, poolSize={}, count={}", diff, pool.size(), want);
    return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_DIFFICULTY", req.userId(), pickMcq(pool, want), "WRITTEN");
  }

  /* ================= 약점 보완 ================= */
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(AssistDtos.WeaknessStartReq req) {
    int want = sanitizeCount(req.count());

    List<UserProgress> progresses = progressRepository.findByUserId(req.userId());
    progresses.sort(Comparator.comparingDouble(this::writtenAccuracy)
        .thenComparing(UserProgress::getUpdatedAt));

    List<Long> targetTopics = progresses.stream()
        .map(UserProgress::getTopicId)
        .limit(5)
        .toList();

    List<Question> pool;
    if (targetTopics.isEmpty()) {
      pool = questionRepository
          .findByModeAndTypeAndDifficulty(ExamMode.WRITTEN, QuestionType.MCQ, Difficulty.NORMAL);
    } else {
      pool = questionRepository
          .findByTopicIdInAndModeAndType(targetTopics, ExamMode.WRITTEN, QuestionType.MCQ);
    }
    pool = pool.stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/weakness] userId={}, targets={}, poolSize={}, count={}",
        req.userId(), targetTopics, pool.size(), want);

      return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_WEAKNESS", req.userId(), pickMcq(pool, want), "WRITTEN");
  }

  /* ================= 내부 유틸 ================= */

  private AssistDtos.QuizSet pickMcq(List<Question> pool, int count) {
    if (pool == null || pool.isEmpty()) {
      log.debug("[assist/written/pickMcq] empty pool");
      return new AssistDtos.QuizSet(List.of());
    }

    // 중복 제거 후 셔플
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);

    // 널-세이프 라벨 정렬기
    Comparator<QuestionChoice> byLabelNullSafe =
        Comparator.comparing(QuestionChoice::getLabel, Comparator.nullsLast(String::compareTo));

    int lim = Math.min(copy.size(), Math.max(1, count));
    List<AssistDtos.QuizQ> items = new ArrayList<>(lim);

    for (Question q : copy.stream().limit(lim).toList()) {
      List<QuestionChoice> raw = Optional.ofNullable(choiceRepository.findByQuestionId(q.getId())).orElse(List.of());

      List<AssistDtos.Choice> choices = raw.stream()
          .filter(Objects::nonNull)
          .sorted(byLabelNullSafe)
          .map(c -> new AssistDtos.Choice(
              Optional.ofNullable(c.getLabel()).orElse(""),
              Optional.ofNullable(c.getContent()).orElse("")
          ))
          .toList();

      items.add(new AssistDtos.QuizQ(
          q.getId(),
          Optional.ofNullable(q.getStem()).orElse(""),
          choices,
          q.getImageUrl()
      ));
    }

    return new AssistDtos.QuizSet(items);
  }

  /** 10/20/50 중 가장 가까운 값으로 보정 (미지정/이상치 방지) */
  private int sanitizeCount(Integer v) {
    if (v == null) return 20;
    if (ALLOWED_COUNTS.contains(v)) return v;
    return ALLOWED_COUNTS.stream()
        .min(Comparator.comparingInt(a -> Math.abs(a - v)))
        .orElse(20);
  }

  /** writtenAccuracy(%) 기반 휴리스틱. */
  private double writtenAccuracy(UserProgress progress) {
    return Optional.ofNullable(progress.getWrittenAccuracy()).orElse(0.0);
  }

  private FlowDtos.StepEnvelope<AssistDtos.QuizSet> wrap(String mode,
                                                         String step,
                                                         String userId,
                                                         AssistDtos.QuizSet payload,
                                                         String reportMode) {
    Map<String, Object> meta = fetchStats(userId, reportMode);
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

  private Map<String, Object> fetchStats(String userId, String reportMode) {
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
    } catch (Exception ex) {
      log.debug("Failed to fetch today goal for {}: {}", userId, ex.getMessage());
    }
    try {
      ProgressQueryClient.Overview overview = progressQueryClient.overview(userId, reportMode);
      if (overview != null) {
        meta.put("weeklySolved", overview.problemsThisWeek());
        meta.put("avgAccuracy", overview.avgAccuracy());
      }
    } catch (Exception ex) {
      log.debug("Failed to fetch overview for {}: {}", userId, ex.getMessage());
    }
    return meta;
  }
}
