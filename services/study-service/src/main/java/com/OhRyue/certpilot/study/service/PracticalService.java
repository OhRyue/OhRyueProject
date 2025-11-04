package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.dto.QuizDtos;
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

    private static final int PRACTICAL_SET_COUNT = 5;

    private final QuestionRepository qRepo;
    private final UserAnswerRepository ansRepo;
    private final UserProgressRepository progressRepo;
    private final AIExplanationService ai;
    private final TopicTreeService topicTree;

    public PracticalSet practicalSet(Long topicId, Integer countOpt) {
        List<Question> pool = qRepo.findAll().stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .filter(this::isPractical)
                .sorted(Comparator.comparingLong(Question::getId))
                .collect(Collectors.toList());

        Collections.shuffle(pool);
        List<Question> pick = pool.stream().limit(PRACTICAL_SET_COUNT).toList();

        var items = pick.stream()
                .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
                .toList();

        return new PracticalSet(items);
    }

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

            var aiRes = ai.explainAndScorePractical(q.getType().name(), q, a.userText());
            int score = Optional.ofNullable(aiRes.score()).orElse(0);
            sum += score;

            boolean correct = score >= 60;
            if (!correct) wrongIds.add(q.getId());

            ansRepo.save(UserAnswer.builder()
                    .userId(req.userId())
                    .questionId(q.getId())
                    .correct(correct)
                    .score(score)
                    .answerText(nz(a.userText()))
                    .createdAt(Instant.now())
                    .build());

            items.add(new PracticalSubmitItem(q.getId(), score, nz(q.getExplanation()), aiRes.explanation()));
        }

        int total = items.size();
        int avg = (total == 0) ? 0 : Math.round(sum * 1f / total);

        UserProgress p = progressRepo
                .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.PRACTICAL)
                .orElseGet(() -> UserProgress.builder()
                        .userId(req.userId())
                        .topicId(req.topicId())
                        .examMode(ExamMode.PRACTICAL)
                        .build());
        p.setUpdatedAt(Instant.now());
        progressRepo.save(p);

        // ✅ (4) wrongQuestionIds 포함 반환
        return new PracticalSubmitResp(total, avg, items, wrongIds);
    }

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

        int avgScore = computeAvgScore(userId, topicId);
        int totalSolved = (int) ansRepo.findAll().stream()
                .filter(a -> Objects.equals(a.getUserId(), userId))
                .map(UserAnswer::getQuestionId)
                .map(qRepo::findById)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .count();

        boolean completed = (avgScore > 0 || totalSolved > 0);

        int streak = computeStreakDays(userId);
        String aiSummary = ai.summarizePracticalKorean(userId, topicId, totalSolved, avgScore, streak);

        return new SummaryResp(
                miniT,
                miniC,
                Boolean.TRUE.equals(p.isMiniPassed()),
                0,
                0,
                aiSummary,
                completed
        );
    }

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
            if (!Objects.equals(qOpt.get().getTopicId(), topicId)) continue;
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
    private double acc(UserProgress p) {
        int total = Math.max(1, Optional.ofNullable(p.getMcqTotal()).orElse(0));
        int correct = Optional.ofNullable(p.getMcqCorrect()).orElse(0);
        return (double) correct / total;
    }
    private QuizDtos.QuizSet pickPractical(List<Question> pool, int count) {
        if (pool.isEmpty()) return new QuizDtos.QuizSet(List.of());
        Collections.shuffle(pool);
        int lim = Math.max(1, count);

        List<QuizDtos.QuizQ> qs = pool.stream().limit(lim)
                .map(q -> new QuizDtos.QuizQ(q.getId(), nz(q.getText()), List.of()))
                .toList();

        return new QuizDtos.QuizSet(qs);
    }
}
