package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.WrittenDtos.SummaryResp;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalAnswer;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalGradeOneReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalGradeOneResp;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalQuestion;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSet;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitItem;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitResp;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class PracticalService {

  private static final int PRACTICAL_SET_COUNT = 5; // 기본: 5문

  private final QuestionRepository qRepo;
  private final UserAnswerRepository ansRepo;
  private final UserProgressRepository progressRepo;
  private final AIExplanationService ai;
  private final TopicTreeService topicTree;

  /* =========================================================
   * (메인) 실기 세트: SHORT/LONG 혼합
   *  - countOpt가 있으면 해당 수, 없으면 기본 5문
   * ========================================================= */
  public PracticalSet practicalSet(Long topicId, Integer countOpt) {
    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(this::isPractical)
        .sorted(Comparator.comparingLong(Question::getId))
        .collect(Collectors.toList());

    Collections.shuffle(pool);

    int count = (countOpt == null) ? PRACTICAL_SET_COUNT : Math.max(1, countOpt);
    List<Question> pick = pool.stream().limit(count).toList();

    var items = pick.stream()
        .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
        .toList();

    return new PracticalSet(items);
  }

  /* =========================================================
   * (메인) 실기 제출/채점 (AI)
   *  - SHORT/LONG만 허용
   *  - 60점 미만은 오답으로 wrongQuestionIds에 포함
   * ========================================================= */
  public PracticalSubmitResp submitPractical(PracticalSubmitReq req) {
    Map<Long, Question> byId = qRepo.findAllById(
            req.answers().stream().map(PracticalAnswer::questionId).toList())
        .stream().collect(Collectors.toMap(Question::getId, it -> it));

    int sum = 0;
    List<PracticalSubmitItem> items = new ArrayList<>();
    List<Long> wrongIds = new ArrayList<>();

    for (var a : req.answers()) {
      Question q = byId.get(a.questionId());
      if (q == null) continue;

      if (!isPractical(q)) {
        throw new IllegalArgumentException("Practical submit only accepts SHORT/LONG questions.");
      }

      var aiRes = ai.explainAndScorePractical(q.getType().name(), q, a.userText());
      int score = Optional.ofNullable(aiRes.score()).orElse(0);
      if (score < 60) wrongIds.add(q.getId());

      sum += score;

      ansRepo.save(UserAnswer.builder()
          .userId(req.userId())
          .questionId(q.getId())
          .correct(score >= 60) // 정책: 60점 이상 PASS
          .score(score)
          .answerText(nz(a.userText()))
          .createdAt(Instant.now())
          .build());

      items.add(new PracticalSubmitItem(q.getId(), score, nz(q.getExplanation()), aiRes.explanation()));
    }

    int total = items.size();
    int avg = (total == 0) ? 0 : Math.round(sum * 1f / total);

    // 진행도 최신화(완료 판정은 summary에서)
    UserProgress p = progressRepo
        .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.PRACTICAL)
        .orElseGet(() -> UserProgress.builder()
            .userId(req.userId())
            .topicId(req.topicId())
            .examMode(ExamMode.PRACTICAL)
            .build());
    p.setUpdatedAt(Instant.now());
    progressRepo.save(p);

    return new PracticalSubmitResp(total, avg, items, wrongIds);
  }

  /* =========================================================
   * (리뷰) 실기 리뷰(총정리) 20문 (SHORT/LONG만)
   * ========================================================= */
  @Transactional(Transactional.TxType.SUPPORTS)
  public PracticalSet practicalReviewSet(Long rootTopicId) {
    Set<Long> topicIds = topicTree.descendantIds(rootTopicId);
    List<Question> pool = qRepo.findAll().stream()
        .filter(q -> topicIds.contains(q.getTopicId()))
        .filter(this::isPractical)
        .collect(Collectors.toList());
    Collections.shuffle(pool);
    List<Question> pick = pool.stream().limit(20).toList();

    var items = pick.stream()
        .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
        .toList();
    return new PracticalSet(items);
  }

  /* =========================================================
   * 실기 진행 요약
   *  - 완료 정책: 미니 통과 && (해당 토픽의 실기 최신 풀이가 ≥ 1) && (해당 토픽 실기 최신 점수 모두 ≥ 60)
   *  - avgScore/totalSolved도 실기 유형만 집계
   * ========================================================= */
  @Transactional(Transactional.TxType.SUPPORTS)
  public SummaryResp summary(String userId, Long topicId) {
    var p = progressRepo.findByUserIdAndTopicIdAndExamMode(userId, topicId, ExamMode.PRACTICAL)
        .orElseGet(() -> UserProgress.builder()
            .userId(userId)
            .topicId(topicId)
            .examMode(ExamMode.PRACTICAL)
            .build());

    int miniT = nz(p.getMiniTotal());
    int miniC = nz(p.getMiniCorrect());
    boolean miniPassed = Boolean.TRUE.equals(p.isMiniPassed());

    // 사용자별 최신 답변(문항 단위 최신 1개)을 모으고, topicId 범위의 실기 문항에 대해 모두 60점 이상인지 검사
    var latestPerQ = ansRepo.findAll().stream()
        .filter(a -> Objects.equals(a.getUserId(), userId))
        .collect(Collectors.groupingBy(
            UserAnswer::getQuestionId,
            Collectors.maxBy(Comparator.comparing(UserAnswer::getCreatedAt))
        ));

    boolean allPassInTopic = true;
    int practicalAnsweredCount = 0;

    for (var e : latestPerQ.entrySet()) {
      var latestAnsOpt = e.getValue();
      if (latestAnsOpt.isEmpty()) continue;

      Long qid = e.getKey();
      var qOpt = qRepo.findById(qid);
      if (qOpt.isEmpty()) continue;
      var q = qOpt.get();

      // 해당 토픽 + 실기 유형만 대상
      if (!Objects.equals(q.getTopicId(), topicId)) continue;
      if (!isPractical(q)) continue;

      practicalAnsweredCount++;
      Integer sc = latestAnsOpt.get().getScore();
      if (sc != null && sc < 60) {
        allPassInTopic = false;
        break;
      }
    }

    boolean completed = miniPassed && (practicalAnsweredCount > 0) && allPassInTopic;

    // 부가 요약(평균점/풀이 수/연속일/aiSummary) — 실기 유형만 집계
    int avgScore = computeAvgScore(userId, topicId);
    int totalSolved = (int) ansRepo.findAll().stream()
        .filter(a -> Objects.equals(a.getUserId(), userId))
        .map(UserAnswer::getQuestionId)
        .map(qRepo::findById)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .filter(q -> Objects.equals(q.getTopicId(), topicId))
        .filter(this::isPractical)
        .count();

    int streak = computeStreakDays(userId);
    String aiSummary = ai.summarizePracticalKorean(userId, topicId, totalSolved, avgScore, streak);

    return new SummaryResp(
        miniT,
        miniC,
        miniPassed,
        0,      // 실기는 객관식 수치 비노출(필드는 호환 유지)
        0,
        aiSummary,
        completed
    );
  }

  /* ===================== 단건 즉시 채점 ===================== */
  @Transactional
  public PracticalGradeOneResp gradeOnePractical(PracticalGradeOneReq req) {
    PracticalSubmitReq batch = new PracticalSubmitReq(
        req.userId(),
        req.topicId(),
        List.of(new PracticalAnswer(req.questionId(), req.userText()))
    );
    PracticalSubmitResp r = submitPractical(batch);

    PracticalSubmitItem item = r.items().isEmpty()
        ? new PracticalSubmitItem(req.questionId(), 0, "", "")
        : r.items().get(0);

    return new PracticalGradeOneResp(item.score(), item.baseExplanation(), item.aiExplanation());
  }

  /* ================= 내부 유틸 ================= */

  private int computeAvgScore(String userId, Long topicId) {
    var latestPerQ = ansRepo.findAll().stream()
        .filter(a -> Objects.equals(a.getUserId(), userId))
        .collect(Collectors.groupingBy(
            UserAnswer::getQuestionId,
            Collectors.maxBy(Comparator.comparing(UserAnswer::getCreatedAt))
        ));

    List<Integer> scores = new ArrayList<>();
    for (var e : latestPerQ.entrySet()) {
      Long qid = e.getKey();
      var optAns = e.getValue();
      if (optAns.isEmpty()) continue;

      var qOpt = qRepo.findById(qid);
      if (qOpt.isEmpty()) continue;
      var q = qOpt.get();

      if (!Objects.equals(q.getTopicId(), topicId)) continue;
      if (!isPractical(q)) continue; // 실기 유형만 평균에 포함

      scores.add(Optional.ofNullable(optAns.get().getScore()).orElse(0));
    }
    if (scores.isEmpty()) return 0;
    return (int) Math.round(scores.stream().mapToInt(Integer::intValue).average().orElse(0.0));
  }

  private int computeStreakDays(String userId) {
    ZoneId KST = ZoneId.of("Asia/Seoul");
    Set<LocalDate> days = ansRepo.findAll().stream()
        .filter(a -> Objects.equals(a.getUserId(), userId))
        .map(a -> LocalDateTime.ofInstant(a.getCreatedAt(), KST).toLocalDate())
        .collect(Collectors.toSet());
    if (days.isEmpty()) return 0;

    int streak = 0;
    LocalDate cur = LocalDate.now(KST);
    while (days.contains(cur)) { streak++; cur = cur.minusDays(1); }
    return streak;
  }

  private static String nz(String s) { return (s == null) ? "" : s; }
  private static int nz(Integer v) { return v == null ? 0 : v; }

  private boolean isPractical(Question q) {
    return q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG;
  }
}
