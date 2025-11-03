package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.MicroDtos.SummaryResp;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalAnswer;
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

/**
 * 실기(주관식) 메인 흐름 서비스
 * - 세트: SHORT/LONG 혼합 출제
 * - 제출: AI 채점(0~100) + 맞춤 해설 저장/반환
 * - 리뷰(총정리): 루트 토픽 이하 SHORT/LONG 20문
 * - 요약: 평균점수/풀이개수/연속일/aiSummary/완료여부
 *
 * 주의:
 * - "미니체크(OX)"는 필기 로직 재사용(컨트롤러에서 StudyFlowService 호출)하며,
 *   요구사항에 따라 AI 해설을 사용하지 않으므로 본 서비스에는 포함하지 않습니다.
 * - "틀린문제 다시보기(리캡)"은 StudyFlowService.wrongRecap(...) 사용(재풀이 아님).
 */
@Service
@RequiredArgsConstructor
@Transactional
public class PracticalService {

    private final QuestionRepository qRepo;
    private final UserAnswerRepository ansRepo;
    private final UserProgressRepository progressRepo;
    private final AIExplanationService ai;
    private final TopicTreeService topicTree;

    /* =========================================================
     * (메인) 실기 세트: SHORT/LONG 혼합 출제
     * ========================================================= */
    public PracticalSet practicalSet(Long topicId, Integer countOpt) {
        int count = (countOpt == null || countOpt < 1) ? 4 : Math.min(10, countOpt);

        List<Question> pool = qRepo.findAll().stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
                .sorted(Comparator.comparingLong(Question::getId))
                .collect(Collectors.toList());

        Collections.shuffle(pool);
        List<Question> pick = pool.stream().limit(count).toList();

        var items = pick.stream()
                .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
                .toList();

        return new PracticalSet(items);
    }

    /* =========================================================
     * (메인) 실기 제출/채점: AI 채점 + 맞춤 해설
     *   - score >= 60 → correct=true 저장(오답/리캡/통계용)
     *   - 진행도 레코드 업데이트(최신화)
     * ========================================================= */
    public PracticalSubmitResp submitPractical(PracticalSubmitReq req) {
        Map<Long, Question> byId = qRepo.findAllById(
                        req.answers().stream().map(PracticalAnswer::questionId).toList())
                .stream().collect(Collectors.toMap(Question::getId, it -> it));

        int sum = 0;
        List<PracticalSubmitItem> items = new ArrayList<>();

        for (var a : req.answers()) {
            Question q = byId.get(a.questionId());
            if (q == null) continue;

            var aiRes = ai.explainAndScorePractical(q.getType().name(), q, a.userText());
            int score = Optional.ofNullable(aiRes.score()).orElse(0);
            sum += score;

            ansRepo.save(UserAnswer.builder()
                    .userId(req.userId())
                    .questionId(q.getId())
                    .correct(score >= 60) // 정책: 60점 이상 PASS로 적재
                    .score(score)
                    .answerText(nz(a.userText()))
                    .createdAt(Instant.now())
                    .build());

            items.add(new PracticalSubmitItem(q.getId(), score, nz(q.getExplanation()), aiRes.explanation()));
        }

        int total = items.size();
        int avg = (total == 0) ? 0 : Math.round(sum * 1f / total);

        // 진행도 레코드만 최신화(완료 판정은 summary에서)
        UserProgress p = progressRepo
                .findByUserIdAndTopicIdAndExamMode(req.userId(), req.topicId(), ExamMode.PRACTICAL)
                .orElseGet(() -> UserProgress.builder()
                        .userId(req.userId())
                        .topicId(req.topicId())
                        .examMode(ExamMode.PRACTICAL)
                        .build());
        p.setUpdatedAt(Instant.now());
        progressRepo.save(p);

        return new PracticalSubmitResp(total, avg, items);
    }

    /* =========================================================
     * (리뷰) 실기 리뷰(총정리) 20문: 루트 이하 SHORT/LONG에서 랜덤 20
     *   - 제출/채점은 submitPractical 재사용(AI 채점/해설)
     * ========================================================= */
    @Transactional(Transactional.TxType.SUPPORTS)
    public PracticalSet practicalReviewSet(Long rootTopicId) {
        Set<Long> topicIds = topicTree.descendantIds(rootTopicId);
        List<Question> pool = qRepo.findAll().stream()
                .filter(q -> topicIds.contains(q.getTopicId()))
                .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
                .collect(Collectors.toList());
        Collections.shuffle(pool);
        List<Question> pick = pool.stream().limit(20).toList();

        var items = pick.stream()
                .map(q -> new PracticalQuestion(q.getId(), q.getType().name(), nz(q.getText()), q.getImageUrl()))
                .toList();
        return new PracticalSet(items);
    }

    /** (선택) 리뷰 제출이 분리되어야 하면 컨트롤러에서 이 메서드를 직접 호출하도록 둘 수 있습니다. */
    public PracticalSubmitResp practicalReviewSubmit(PracticalSubmitReq req) {
        return submitPractical(req);
    }

    /* =========================================================
     * (요약) 실기 진행 요약
     *  - mini: 필기 미니체크를 실기에선 사용하지 않으므로 저장 값만 반영(있으면)
     *  - mcqTotal/mcqCorrect: 실기에선 0으로 통일(포맷 정합)
     *  - aiSummary: LLM 요약(실패 시 폴백 문구를 AIExplanationService에서 구성)
     *  - completed: 최소 조건(예: 평균 점수>0 등)으로 간단 판정
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

        int avgScore = computeAvgScore(userId, topicId);
        int totalSolved = (int) ansRepo.findAll().stream()
                .filter(a -> Objects.equals(a.getUserId(), userId))
                .map(UserAnswer::getQuestionId)
                .map(qRepo::findById)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .count();

        // 완료 판정(간단 기준): 해당 토픽에서 채점된 기록이 하나라도 있으면 완료
        boolean completed = (avgScore > 0 || totalSolved > 0);

        int streak = computeStreakDays(userId);
        String aiSummary = ai.summarizePracticalKorean(userId, topicId, totalSolved, avgScore, streak);

        // 실기는 mcq 지표를 0으로 반환(포맷 통일)
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
}
