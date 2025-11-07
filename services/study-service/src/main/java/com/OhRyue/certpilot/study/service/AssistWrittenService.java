package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 보조학습 - 필기(MCQ) 전용 서비스
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistWrittenService {

  private static final List<Integer> ALLOWED_COUNTS = List.of(10, 20, 50);

  private final QuestionRepository qRepo;
  private final QuestionChoiceRepository choiceRepo;
  private final UserProgressRepository progressRepo;

  /* ================= 카테고리 ================= */
  public AssistDtos.QuizSet startByCategory(AssistDtos.CategoryStartReq req) {
    List<Long> topicIds = Optional.ofNullable(req.topicIds()).orElse(List.of());
    int want = sanitizeCount(req.count());

    if (topicIds.isEmpty()) {
      log.debug("[assist/written/category] empty topicIds -> empty set");
      return new AssistDtos.QuizSet(List.of());
    }

    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> topicIds.contains(q.getTopicId()))
        .filter(q -> q.getType() == QuestionType.MCQ)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    log.debug("[assist/written/category] topicIds={}, poolSize={}, count={}", topicIds, pool.size(), want);
    return pickMcq(pool, want);
  }

  /* ================= 난이도 ================= */
  public AssistDtos.QuizSet startByDifficulty(AssistDtos.DifficultyStartReq req) {
    Difficulty diff = Objects.requireNonNullElse(req.difficulty(), Difficulty.NORMAL);
    int want = sanitizeCount(req.count());

    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> q.getType() == QuestionType.MCQ)
        .filter(q -> Objects.equals(q.getDifficulty(), diff))
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    log.debug("[assist/written/difficulty] diff={}, poolSize={}, count={}", diff, pool.size(), want);
    return pickMcq(pool, want);
  }

  /* ================= 약점 보완 ================= */
  public AssistDtos.QuizSet startByWeakness(AssistDtos.WeaknessStartReq req) {
    int want = sanitizeCount(req.count());

    // 휴리스틱: MCQ 정확도 낮은 토픽 우선
    List<UserProgress> ps = progressRepo.findByUserId(req.userId());
    ps.sort(Comparator.comparingDouble(this::acc).thenComparing(UserProgress::getUpdatedAt));

    List<Long> targetTopics = ps.stream().map(UserProgress::getTopicId).limit(5).toList();

    List<Question> pool = (targetTopics.isEmpty()
        ? qRepo.findAll().stream()
        .filter(q -> q.getType() == QuestionType.MCQ)
        .filter(q -> Objects.equals(q.getDifficulty(), Difficulty.NORMAL))
        : qRepo.findAll().stream()
        .filter(q -> q.getType() == QuestionType.MCQ)
        .filter(q -> targetTopics.contains(q.getTopicId()))
    ).sorted(Comparator.comparingLong(Question::getId)).collect(Collectors.toList());

    log.debug("[assist/written/weakness] userId={}, targets={}, poolSize={}, count={}",
        req.userId(), targetTopics, pool.size(), want);

    return pickMcq(pool, want);
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
      List<QuestionChoice> raw = Optional.ofNullable(choiceRepo.findByQuestionId(q.getId())).orElse(List.of());

      List<AssistDtos.Choice> choices = raw.stream()
          .filter(Objects::nonNull)
          .sorted(byLabelNullSafe)
          .map(c -> new AssistDtos.Choice(nz(c.getLabel()), nz(c.getText())))
          .toList();

      items.add(new AssistDtos.QuizQ(q.getId(), nz(q.getText()), choices, q.getImageUrl()));
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

  /** MCQ 정확도 (correct / total). total=0 방지 */
  private double acc(UserProgress p) {
    int total = Math.max(1, nz(p.getMcqTotal()));
    int correct = nz(p.getMcqCorrect());
    return (double) correct / total;
  }

  private static int nz(Integer v) { return v == null ? 0 : v; }
  private static String nz(String s) { return s == null ? "" : s; }
}
