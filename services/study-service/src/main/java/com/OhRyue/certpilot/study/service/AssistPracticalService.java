package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

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

  private final QuestionRepository qRepo;
  private final UserProgressRepository progressRepo;

  /* ================= 카테고리 ================= */
  public AssistDtos.QuizSet startByCategory(AssistDtos.CategoryStartReq req) {
    List<Long> topicIds = Optional.ofNullable(req.topicIds()).orElse(List.of());
    if (topicIds.isEmpty()) return new AssistDtos.QuizSet(List.of());

    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> topicIds.contains(q.getTopicId()))
        .filter(this::isPractical)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    return pickPractical(pool, sanitizeCount(req.count()));
  }

  /* ================= 난이도 ================= */
  public AssistDtos.QuizSet startByDifficulty(AssistDtos.DifficultyStartReq req) {
    Difficulty diff = Objects.requireNonNullElse(req.difficulty(), Difficulty.NORMAL);

    List<Question> pool = qRepo.findAll().stream()
        .filter(this::isPractical)
        .filter(q -> q.getDifficulty() == diff)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    return pickPractical(pool, sanitizeCount(req.count()));
  }

  /* ================= 약점 보완 ================= */
  public AssistDtos.QuizSet startByWeakness(AssistDtos.WeaknessStartReq req) {
    // 실기 세밀 지표가 없을 경우, MCQ 정확도 낮은 토픽을 약점으로 간주(일관 휴리스틱)
    List<UserProgress> ps = progressRepo.findByUserId(req.userId());
    ps.sort(Comparator.comparingDouble(this::acc).thenComparing(UserProgress::getUpdatedAt));
    List<Long> targetTopics = ps.stream().map(UserProgress::getTopicId).limit(5).toList();

    List<Question> pool = (targetTopics.isEmpty()
        ? qRepo.findAll().stream()
        .filter(this::isPractical)
        .filter(q -> q.getDifficulty() == Difficulty.NORMAL)
        : qRepo.findAll().stream()
        .filter(this::isPractical)
        .filter(q -> targetTopics.contains(q.getTopicId())))
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    return pickPractical(pool, sanitizeCount(req.count()));
  }

  /* ================= 내부 유틸 ================= */

  private boolean isPractical(Question q) {
    return q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG;
  }

  private AssistDtos.QuizSet pickPractical(List<Question> pool, int count) {
    if (pool.isEmpty()) return new AssistDtos.QuizSet(List.of());

    // 중복 제거 후 셔플
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);

    int lim = Math.min(copy.size(), Math.max(1, count));

    var items = copy.stream().limit(lim)
        .map(q -> new AssistDtos.QuizQ(q.getId(), nz(q.getText()), List.of(), q.getImageUrl()))
        .toList();

    return new AssistDtos.QuizSet(items);
  }

  /** 정확도 (correct / total). total=0 방지 */
  private double acc(UserProgress p) {
    int total = Math.max(1, nz(p.getMcqTotal()));
    int correct = nz(p.getMcqCorrect());
    return (double) correct / total;
  }

  /** 10/20/50 중 가장 가까운 값으로 보정 (미지정/이상치 방지) */
  private int sanitizeCount(Integer v) {
    if (v == null) return 20;
    if (ALLOWED_COUNTS.contains(v)) return v;
    return ALLOWED_COUNTS.stream()
        .min(Comparator.comparingInt(a -> Math.abs(a - v)))
        .orElse(20);
  }

  private static int nz(Integer v) { return v == null ? 0 : v; }
  private static String nz(String s) { return s == null ? "" : s; }
}
