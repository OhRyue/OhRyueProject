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
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 보조학습 - 필기(MCQ) 전용 서비스
 * - 세트 시작: 카테고리 / 난이도 / 약점 보완
 * - 제출: Assist 전용 DTO 사용, 단순 채점(세션/XP는 별도)
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistWrittenService {

    // 스펙 v1.0: 문제 수 5/10/20 (기존 10/20/50에서 변경)
    private static final List<Integer> ALLOWED_COUNTS = List.of(5, 10, 20);

    private final QuestionRepository questionRepository;
    private final QuestionChoiceRepository choiceRepository;
    private final UserProgressRepository progressRepository;
    private final ProgressQueryClient progressQueryClient;
    private final TopicTreeService topicTreeService;
    // 스펙 v1.0: StudySession 관리 및 XP 지급을 위한 의존성 추가
    private final com.OhRyue.certpilot.study.repository.UserAnswerRepository userAnswerRepository;
    private final StudySessionManager sessionManager;
    private final com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
    private final AIExplanationService aiExplanationService;
    private final ObjectMapper objectMapper;

    /* ================= 카테고리: 2레벨 토픽 선택 → 하위 토픽 전체에서 출제 ================= */

    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(String userId,
                                                                     Long rootTopicId,
                                                                     Integer count) {
        int want = sanitizeCount(count);

        // rootTopicId 포함 + 모든 하위 토픽 ID
        Set<Long> topicIds = topicTreeService.descendantsOf(rootTopicId);
        if (topicIds.isEmpty()) {
            topicIds = Set.of(rootTopicId);
        }

        List<Question> pool = questionRepository
                .findByTopicIdInAndModeAndType(topicIds, ExamMode.WRITTEN, QuestionType.MCQ)
                .stream()
                .sorted(Comparator.comparingLong(Question::getId))
                .toList();

        log.debug("[assist/written/category] rootTopicId={}, topicIds={}, poolSize={}, count={}",
                rootTopicId, topicIds, pool.size(), want);

        AssistDtos.QuizSet set = pickMcq(pool, want);
        return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_CATEGORY", userId, set, "WRITTEN");
    }

    /* ================= 난이도 ================= */

    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(String userId,
                                                                       Difficulty diff,
                                                                       Integer count) {
        Difficulty difficulty = (diff == null ? Difficulty.NORMAL : diff);
        int want = sanitizeCount(count);

        List<Question> pool = questionRepository
                .findByModeAndTypeAndDifficulty(ExamMode.WRITTEN, QuestionType.MCQ, difficulty)
                .stream()
                .sorted(Comparator.comparingLong(Question::getId))
                .toList();

        log.debug("[assist/written/difficulty] diff={}, poolSize={}, count={}",
                difficulty, pool.size(), want);

        AssistDtos.QuizSet set = pickMcq(pool, want);
        return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_DIFFICULTY", userId, set, "WRITTEN");
    }

    /* ================= 약점 보완 ================= */

    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(String userId,
                                                                     Integer count) {
        int want = sanitizeCount(count);

        List<UserProgress> progresses = progressRepository.findByUserId(userId);
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
                userId, targetTopics, pool.size(), want);

        AssistDtos.QuizSet set = pickMcq(pool, want);
        return wrap("ASSIST_WRITTEN", "ASSIST_WRITTEN_WEAKNESS", userId, set, "WRITTEN");
    }

    /* ================= 제출(필기 – MCQ) ================= */
    // 스펙 v1.0: StudySession 기록, UserAnswer 저장, 틀린 문제 수집, XP 지급

    @Transactional
    public FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submit(AssistDtos.WrittenSubmitReq req) {
        if (req == null || req.answers() == null || req.answers().isEmpty()) {
            return new FlowDtos.StepEnvelope<>(
                    null,
                    "ASSIST_WRITTEN",
                    "ASSIST_WRITTEN_SUBMIT",
                    "COMPLETE",
                    null,
                    fetchStats(req != null ? req.userId() : null, "WRITTEN"),
                    new AssistDtos.WrittenSubmitResp(0, 0, List.of())
            );
        }

        // 문제 캐시
        List<Long> qIds = req.answers().stream()
                .map(AssistDtos.WrittenAnswer::questionId)
                .filter(Objects::nonNull)
                .toList();

        Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
                .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
                .collect(java.util.stream.Collectors.toMap(Question::getId, q -> q));

        // 스펙 v1.0: Assist용 StudySession 생성 (mode: ASSIST_CATEGORY/ASSIST_DIFFICULTY/ASSIST_WEAK)
        // scope에는 assistType 정보 포함 (카테고리면 rootTopicId, 난이도면 difficulty, 약점이면 "WEAK")
        String assistMode = "ASSIST_CATEGORY"; // 기본값, 실제로는 req에서 assistType을 받아야 함
        Map<String, Object> scope = Map.of("assistType", assistMode, "questionCount", req.answers().size());
        com.OhRyue.certpilot.study.domain.StudySession session = sessionManager.ensureSession(
                req.userId(), scope, assistMode, ExamMode.WRITTEN, req.answers().size());

        List<AssistDtos.WrittenResultItem> items = new ArrayList<>();
        List<Long> wrongQuestionIds = new ArrayList<>();
        int correct = 0;
        int orderNo = 0;

        for (AssistDtos.WrittenAnswer ans : req.answers()) {
            Question q = questionMap.get(ans.questionId());
            if (q == null) {
                continue;
            }
            orderNo++;
            String userLabel = Optional.ofNullable(ans.label()).orElse("").trim();
            String correctLabel = Optional.ofNullable(q.getAnswerKey()).orElse("").trim();
            boolean isCorrect = !correctLabel.isBlank() && correctLabel.equalsIgnoreCase(userLabel);
            if (isCorrect) {
                correct++;
            } else {
                wrongQuestionIds.add(q.getId());
            }

            // 스펙 v1.0: 오답은 AI 해설 추가
            String explanation = Optional.ofNullable(q.getSolutionText()).orElse("");
            String aiExplanation = "";
            if (!isCorrect) {
                try {
                    aiExplanation = aiExplanationService.explainWrongForMCQ(userLabel, correctLabel, q);
                } catch (Exception e) {
                    log.debug("AI explanation failed for question {}: {}", q.getId(), e.getMessage());
                }
            }

            items.add(new AssistDtos.WrittenResultItem(
                    q.getId(),
                    isCorrect,
                    correctLabel,
                    explanation + (aiExplanation.isBlank() ? "" : "\n\n[AI 해설]\n" + aiExplanation)
            ));

            // 스펙 v1.0: StudySessionItem 및 UserAnswer 저장
            Map<String, Object> answerPayload = Map.of("label", userLabel, "correct", isCorrect);
            String answerJson = toJson(answerPayload);
            String aiExplainJson = aiExplanation.isBlank() ? null : toJson(Map.of("explain", aiExplanation));
            
            com.OhRyue.certpilot.study.domain.StudySessionItem item = sessionManager.upsertItem(
                    session, q.getId(), orderNo, answerJson, isCorrect, isCorrect ? 100 : 0, aiExplainJson);

            // UserAnswer 저장
            com.OhRyue.certpilot.study.domain.UserAnswer userAnswer = com.OhRyue.certpilot.study.domain.UserAnswer.builder()
                    .userId(req.userId())
                    .questionId(q.getId())
                    .examMode(ExamMode.WRITTEN)
                    .questionType(QuestionType.MCQ)
                    .userAnswerJson(answerJson)
                    .correct(isCorrect)
                    .score(isCorrect ? 100 : 0)
                    .source("ASSIST_WRITTEN")
                    .sessionId(session.getId())
                    .sessionItemId(item.getId())
                    .build();
            userAnswerRepository.save(userAnswer);
            
            // Progress hook (통계용)
            try {
                progressHookClient.submit(new com.OhRyue.certpilot.study.client.ProgressHookClient.SubmitPayload(
                        req.userId(), ExamMode.WRITTEN.name(), QuestionType.MCQ.name(),
                        isCorrect, isCorrect ? 100 : 0, List.of(), "ASSIST_WRITTEN"));
            } catch (Exception e) {
                log.debug("Progress hook failed: {}", e.getMessage());
            }
        }

        // 스펙 v1.0: 세션 완료 처리 및 XP 지급
        boolean allCorrect = !items.isEmpty() && wrongQuestionIds.isEmpty();
        double scorePct = items.isEmpty() ? 0.0 : (correct * 100.0) / items.size();
        sessionManager.closeSession(session, scorePct, allCorrect, Map.of(
                "total", items.size(),
                "correct", correct,
                "wrongQuestionIds", wrongQuestionIds
        ));

        // 스펙 v1.0: passed=true일 때만 XP 지급, 세션당 1회만
        if (allCorrect && !Boolean.TRUE.equals(session.getXpGranted())) {
            try {
                progressHookClient.flowComplete(new com.OhRyue.certpilot.study.client.ProgressHookClient.FlowCompletePayload(
                        req.userId(),
                        ExamMode.WRITTEN.name(),
                        "ASSIST",
                        null // Assist는 topicId 없음
                ));
                sessionManager.markXpGranted(session);
            } catch (Exception e) {
                log.debug("XP grant failed for assist session {}: {}", session.getId(), e.getMessage());
            }
        }

        AssistDtos.WrittenSubmitResp payload =
                new AssistDtos.WrittenSubmitResp(req.answers().size(), correct, items);

        return new FlowDtos.StepEnvelope<>(
                session.getId(), // 스펙 v1.0: sessionId 반환 (틀린 문제 다시보기용)
                "ASSIST_WRITTEN",
                "ASSIST_WRITTEN_SUBMIT",
                "COMPLETE",
                null,
                fetchStats(req.userId(), "WRITTEN"),
                payload
        );
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

        int lim = Math.min(copy.size(), Math.max(1, count));
        List<AssistDtos.QuizQ> items = new ArrayList<>(lim);

        for (Question q : copy.subList(0, lim)) {
            List<QuestionChoice> raw = Optional.ofNullable(
                    choiceRepository.findByQuestionId(q.getId())
            ).orElse(List.of());

            // 널-세이프 라벨 정렬기
            Comparator<QuestionChoice> byLabelNullSafe =
                    Comparator.comparing(QuestionChoice::getLabel,
                            Comparator.nullsLast(String::compareTo));

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

    /** 5/10/20 중 가장 가까운 값으로 보정 (미지정/이상치 방지) - 스펙 v1.0 */
    private int sanitizeCount(Integer v) {
        if (v == null) return 10;
        if (ALLOWED_COUNTS.contains(v)) return v;
        return ALLOWED_COUNTS.stream()
                .min(Comparator.comparingInt(a -> Math.abs(a - v)))
                .orElse(10);
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

    private String toJson(Map<String, Object> payload) {
        try {
            return objectMapper.writeValueAsString(payload);
        } catch (JsonProcessingException e) {
            return "{}";
        }
    }
}
