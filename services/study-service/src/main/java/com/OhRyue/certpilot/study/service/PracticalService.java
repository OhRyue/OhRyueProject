package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.UserAnswer;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.PracticalDtos;
import com.OhRyue.certpilot.study.dto.WrittenDtos;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class PracticalService {

    private static final int MINI_SIZE = 4;
    // 실기 micro 세트는 총 5문제 (SHORT 3 + LONG 2)
    private static final int PRACTICAL_SIZE = 5;
    // 실기 리뷰 세트는 총 10문제 (SHORT 6 + LONG 4)
    private static final int REVIEW_SIZE = 10;

    private final QuestionRepository questionRepository;
    private final QuestionTagRepository questionTagRepository;
    private final UserAnswerRepository userAnswerRepository;
    private final UserProgressRepository userProgressRepository;
    private final StudySessionManager sessionManager;
    private final AIExplanationService aiExplanationService;
    private final TopicTreeService topicTreeService;
    private final ProgressHookClient progressHookClient;
    private final ObjectMapper objectMapper;

    // cert-service 커리큘럼 연동 (토픽 제목/개념 조회용)
    private final CurriculumGateway curriculumGateway;

    /* ========================= 미니체크(OX) ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> miniSet(String userId, Long topicId) {
        StudySession session = sessionManager.ensureMicroSession(
                userId, topicId, ExamMode.PRACTICAL, MINI_SIZE + PRACTICAL_SIZE);

        Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
        boolean passed = Boolean.TRUE.equals(miniMeta.get("passed"));

        List<Question> questions = questionRepository.pickRandomByTopic(
                topicId, ExamMode.PRACTICAL, QuestionType.OX, PageRequest.of(0, MINI_SIZE));
        List<WrittenDtos.MiniQuestion> items = questions.stream()
                .map(q -> new WrittenDtos.MiniQuestion(q.getId(), Optional.ofNullable(q.getStem()).orElse("")))
                .toList();

        String status = passed ? "COMPLETE" : "IN_PROGRESS";
        // 미니 정답 여부와 관계없이 다음 단계는 항상 PRACTICAL_SET 로 이동 가능
        String next = passed ? "PRACTICAL_SET" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "PRACTICAL",
                "PRACTICAL_MINI",
                status,
                next,
                sessionManager.loadMeta(session),
                new WrittenDtos.MiniSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(WrittenDtos.MiniSubmitReq req) {
        Map<Long, Question> questionMap = questionRepository.findByIdIn(
                        req.answers().stream().map(WrittenDtos.MiniAnswer::questionId).toList())
                .stream()
                .filter(q -> q.getMode() == ExamMode.PRACTICAL && q.getType() == QuestionType.OX)
                .collect(Collectors.toMap(Question::getId, q -> q));

        StudySession session = sessionManager.ensureMicroSession(
                req.userId(), req.topicId(), ExamMode.PRACTICAL, MINI_SIZE + PRACTICAL_SIZE);
        int baseOrder = sessionManager.items(session.getId()).size();

        int correctCount = 0;
        List<WrittenDtos.MiniSubmitItem> items = new ArrayList<>();
        List<Long> wrongIds = new ArrayList<>();

        for (int idx = 0; idx < req.answers().size(); idx++) {
            WrittenDtos.MiniAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null) {
                throw new NoSuchElementException("Invalid OX question: " + answer.questionId());
            }

            String userAnswer = Boolean.TRUE.equals(answer.answer()) ? "O" : "X";
            String correctAnswer = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
            boolean correct = correctAnswer.equalsIgnoreCase(userAnswer);
            if (correct) {
                correctCount++;
            } else {
                wrongIds.add(question.getId());
            }

            String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");
            items.add(new WrittenDtos.MiniSubmitItem(question.getId(), correct, explanation, ""));

            Map<String, Object> answerJson = new HashMap<>();
            answerJson.put("answer", userAnswer);
            answerJson.put("correct", correct);
            answerJson.put("submittedAt", Instant.now().toString());

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    toJson(answerJson),
                    correct,
                    correct ? 100 : 0,
                    null
            );

            persistUserAnswer(req.userId(), question, userAnswer, correct, correct ? 100 : 0, session, item, "PRACTICAL_MINI");
            pushProgressHook(req.userId(), QuestionType.OX, correct, correct ? 100 : 0, question.getId());
        }

        boolean passedNow = wrongIds.isEmpty() && !items.isEmpty();

        // 🔹 이전에 한 번이라도 통과했다면 계속 true 유지
        Map<String, Object> prevMiniMeta = sessionManager.loadStepMeta(session, "mini");
        boolean everPassed = Boolean.TRUE.equals(prevMiniMeta.get("passed"));

        Map<String, Object> miniMeta = new HashMap<>(prevMiniMeta);
        miniMeta.put("total", req.answers().size());
        miniMeta.put("correct", correctCount);
        miniMeta.put("passed", everPassed || passedNow);
        miniMeta.put("wrongQuestionIds", wrongIds);
        miniMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "mini", miniMeta);

        if (!everPassed && !passedNow) {
            sessionManager.updateStatus(session, "OPEN");
        }
        // everPassed == true 인 경우는 상태 유지

        boolean finalPassed = everPassed || passedNow;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "PRACTICAL",
                "PRACTICAL_MINI",
                finalPassed ? "COMPLETE" : "IN_PROGRESS",
                // 항상 PRACTICAL_SET 으로 이동 가능하도록 고정
                "PRACTICAL_SET",
                sessionManager.loadMeta(session),
                new WrittenDtos.MiniSubmitResp(req.answers().size(), correctCount, finalPassed, items, wrongIds)
        );
    }

    public WrittenDtos.MiniGradeOneResp gradeOneMini(WrittenDtos.MiniGradeOneReq req) {
        FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> envelope = submitMini(new WrittenDtos.MiniSubmitReq(
                req.userId(),
                req.topicId(),
                List.of(new WrittenDtos.MiniAnswer(req.questionId(), req.answer()))
        ));
        WrittenDtos.MiniSubmitItem item = envelope.payload().items().isEmpty()
                ? new WrittenDtos.MiniSubmitItem(req.questionId(), false, "", "")
                : envelope.payload().items().get(0);
        return new WrittenDtos.MiniGradeOneResp(item.correct(), item.explanation());
    }

    /* ========================= 실기 세트 (Micro) ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<PracticalDtos.PracticalSet> practicalSet(String userId, Long topicId) {
        // SHORT 3 + LONG 2 = 총 5문제
        List<Question> shortQuestions = questionRepository.pickRandomByTopic(
                topicId, ExamMode.PRACTICAL, QuestionType.SHORT, PageRequest.of(0, 3));
        List<Question> longQuestions = questionRepository.pickRandomByTopic(
                topicId, ExamMode.PRACTICAL, QuestionType.LONG, PageRequest.of(0, 2));

        List<Question> combined = Stream.concat(shortQuestions.stream(), longQuestions.stream())
                .distinct()
                .toList();

        List<PracticalDtos.PracticalQuestion> items = combined.stream()
                .map(q -> new PracticalDtos.PracticalQuestion(
                        q.getId(),
                        q.getType().name(),
                        Optional.ofNullable(q.getStem()).orElse(""),
                        q.getImageUrl()))
                .toList();

        StudySession session = sessionManager.ensureMicroSession(
                userId, topicId, ExamMode.PRACTICAL, MINI_SIZE + PRACTICAL_SIZE);

        // 미니 통과 여부와 상관없이 세트 진입 허용
        Map<String, Object> practicalMeta = sessionManager.loadStepMeta(session, "practical");
        boolean completed = Boolean.TRUE.equals(practicalMeta.get("completed"));
        String status = completed ? "COMPLETE" : "IN_PROGRESS";
        String next = completed ? "PRACTICAL_SUMMARY" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "PRACTICAL",
                "PRACTICAL_SET",
                status,
                next,
                sessionManager.loadMeta(session),
                new PracticalDtos.PracticalSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<PracticalDtos.PracticalSubmitResp> submitPractical(PracticalDtos.PracticalSubmitReq req) {
        Map<Long, Question> questionMap = questionRepository.findByIdIn(
                        req.answers().stream().map(PracticalDtos.PracticalAnswer::questionId).toList())
                .stream()
                .filter(q -> q.getMode() == ExamMode.PRACTICAL)
                .collect(Collectors.toMap(Question::getId, q -> q));

        StudySession session = sessionManager.ensureMicroSession(
                req.userId(), req.topicId(), ExamMode.PRACTICAL, MINI_SIZE + PRACTICAL_SIZE);

        // 미니 통과 여부와 관계없이 제출/채점이 항상 가능
        int baseOrder = sessionManager.items(session.getId()).size();

        List<PracticalDtos.PracticalSubmitItem> items = new ArrayList<>();
        List<Long> wrongIds = new ArrayList<>();
        int totalScore = 0;

        for (int idx = 0; idx < req.answers().size(); idx++) {
            PracticalDtos.PracticalAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null || !isPractical(question)) {
                throw new NoSuchElementException("Invalid practical question: " + answer.questionId());
            }

            AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
                    question, answer.userText());
            int score = Optional.ofNullable(result.score()).orElse(0);
            boolean passed = score >= 60;
            totalScore += score;
            if (!passed) wrongIds.add(question.getId());

            items.add(new PracticalDtos.PracticalSubmitItem(
                    question.getId(),
                    score,
                    Optional.ofNullable(question.getSolutionText()).orElse(""),
                    result.explain()
            ));

            Map<String, Object> answerJson = new HashMap<>();
            answerJson.put("answer", Optional.ofNullable(answer.userText()).orElse(""));
            answerJson.put("score", score);
            answerJson.put("passed", passed);
            answerJson.put("tips", result.tips());

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    toJson(answerJson),
                    passed,
                    score,
                    toJson(Map.of("explain", result.explain(), "tips", result.tips()))
            );

            persistUserAnswer(req.userId(), question, answer.userText(), passed, score, session, item, "MICRO_PRACTICAL");
            pushProgressHook(req.userId(), question.getType(), passed, score, question.getId());
            updateProgress(req.userId(), question.getTopicId(), score);
        }

        int total = items.size();
        double avgScore = total == 0 ? 0.0 : totalScore * 1.0 / total;
        boolean allPassedNow = total > 0 && wrongIds.isEmpty();

        // 🔹 이전 메타 불러와서 everCompleted 유지
        Map<String, Object> prevPracticalMeta = sessionManager.loadStepMeta(session, "practical");
        boolean everCompleted = Boolean.TRUE.equals(prevPracticalMeta.get("completed"));
        boolean finalCompleted = everCompleted || allPassedNow;

        Map<String, Object> practicalMeta = new HashMap<>(prevPracticalMeta);
        practicalMeta.put("total", total);
        practicalMeta.put("avgScore", avgScore);
        practicalMeta.put("completed", finalCompleted);
        practicalMeta.put("wrongQuestionIds", wrongIds);
        practicalMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "practical", practicalMeta);

        // 🔹 세션 상태: 한 번 COMPLETE 되면 다시 OPEN 으로 돌리지 않음
        if (!everCompleted && allPassedNow) {
            sessionManager.closeSession(session, avgScore, Map.of("avgScore", avgScore));
        } else if (!everCompleted) {
            sessionManager.updateStatus(session, "OPEN");
        }
        // everCompleted == true 인 경우는 상태 유지

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "PRACTICAL",
                "PRACTICAL_SET",
                finalCompleted ? "COMPLETE" : "IN_PROGRESS",
                finalCompleted ? "PRACTICAL_SUMMARY" : "PRACTICAL_SET",
                sessionManager.loadMeta(session),
                new PracticalDtos.PracticalSubmitResp(
                        total,
                        total == 0 ? 0 : (int) Math.round(avgScore),
                        items,
                        wrongIds
                )
        );
    }

    /* ========================= 실기 리뷰 (Review) ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<PracticalDtos.PracticalSet> practicalReviewSet(String userId, Long rootTopicId) {
        // rootTopicId 포함 + 모든 하위 토픽 id
        Set<Long> topicIds = topicTreeService.descendantsOf(rootTopicId);
        if (topicIds.isEmpty()) topicIds = Set.of(rootTopicId);

        // SHORT 6 + LONG 4 = 총 10문제
        List<Question> shortQuestions = questionRepository.pickRandomByTopicIn(
                topicIds, ExamMode.PRACTICAL, QuestionType.SHORT, PageRequest.of(0, 6));
        List<Question> longQuestions = questionRepository.pickRandomByTopicIn(
                topicIds, ExamMode.PRACTICAL, QuestionType.LONG, PageRequest.of(0, 4));

        List<Question> questions = Stream.concat(shortQuestions.stream(), longQuestions.stream())
                .distinct()
                .toList();

        List<PracticalDtos.PracticalQuestion> items = questions.stream()
                .map(q -> new PracticalDtos.PracticalQuestion(
                        q.getId(),
                        q.getType().name(),
                        Optional.ofNullable(q.getStem()).orElse(""),
                        q.getImageUrl()))
                .toList();

        StudySession session = sessionManager.ensureReviewSession(
                userId, rootTopicId, ExamMode.PRACTICAL, REVIEW_SIZE);
        Map<String, Object> reviewMeta = sessionManager.loadStepMeta(session, "review");
        boolean completed = Boolean.TRUE.equals(reviewMeta.get("completed"));
        String status = completed ? "COMPLETE" : "IN_PROGRESS";
        String next = completed ? "PRACTICAL_REVIEW_SUMMARY" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "REVIEW",
                "PRACTICAL_REVIEW_SET",
                status,
                next,
                sessionManager.loadMeta(session),
                new PracticalDtos.PracticalSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<PracticalDtos.PracticalReviewSubmitResp> practicalReviewSubmit(
            PracticalDtos.PracticalReviewSubmitReq req) {

        // rootTopicId + 하위 토픽 전체를 타겟으로 필터링
        Set<Long> rawIds = topicTreeService.descendantsOf(req.rootTopicId());
        Set<Long> topicIds = new HashSet<>(rawIds);
        if (topicIds.isEmpty()) {
            topicIds.add(req.rootTopicId());
        }
        final Set<Long> targetTopicIds = Set.copyOf(topicIds);

        Map<Long, Question> questionMap = questionRepository.findByIdIn(
                        req.answers().stream().map(PracticalDtos.PracticalAnswer::questionId).toList())
                .stream()
                .filter(q -> q.getMode() == ExamMode.PRACTICAL && targetTopicIds.contains(q.getTopicId()))
                .collect(Collectors.toMap(Question::getId, q -> q));

        StudySession session = sessionManager.ensureReviewSession(
                req.userId(), req.rootTopicId(), ExamMode.PRACTICAL, REVIEW_SIZE);
        int baseOrder = sessionManager.items(session.getId()).size();

        List<PracticalDtos.PracticalSubmitItem> items = new ArrayList<>();
        List<Long> wrongIds = new ArrayList<>();
        int totalScore = 0;

        for (int idx = 0; idx < req.answers().size(); idx++) {
            PracticalDtos.PracticalAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null) {
                continue;
            }

            AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
                    question, answer.userText());
            int score = Optional.ofNullable(result.score()).orElse(0);
            boolean passed = score >= 60;
            totalScore += score;
            if (!passed) {
                wrongIds.add(question.getId());
            }

            items.add(new PracticalDtos.PracticalSubmitItem(
                    question.getId(),
                    score,
                    Optional.ofNullable(question.getSolutionText()).orElse(""),
                    result.explain()
            ));

            Map<String, Object> answerJson = new HashMap<>();
            answerJson.put("answer", Optional.ofNullable(answer.userText()).orElse(""));
            answerJson.put("score", score);
            answerJson.put("passed", passed);
            answerJson.put("tips", result.tips());

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    toJson(answerJson),
                    passed,
                    score,
                    toJson(Map.of("explain", result.explain(), "tips", result.tips()))
            );

            persistUserAnswer(req.userId(), question, answer.userText(), passed, score, session, item, "PRACTICAL_REVIEW");
            pushProgressHook(req.userId(), question.getType(), passed, score, question.getId());
            // 실기 리뷰도 Progress 에 반영
            updateProgress(req.userId(), question.getTopicId(), score);
        }

        int total = items.size();
        double avgScore = total == 0 ? 0.0 : totalScore * 1.0 / total;
        boolean allPassedNow = total > 0 && wrongIds.isEmpty();

        // 🔹 이전 메타 불러와서 everCompleted 유지
        Map<String, Object> prevReviewMeta = sessionManager.loadStepMeta(session, "review");
        boolean everCompleted = Boolean.TRUE.equals(prevReviewMeta.get("completed"));
        boolean finalCompleted = everCompleted || allPassedNow;

        Map<String, Object> reviewMeta = new HashMap<>(prevReviewMeta);
        reviewMeta.put("total", total);
        reviewMeta.put("avgScore", avgScore);
        reviewMeta.put("completed", finalCompleted);
        reviewMeta.put("wrongQuestionIds", wrongIds);
        reviewMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "review", reviewMeta);

        // 🔹 세션 상태: 한 번 COMPLETE 되면 다시 OPEN 으로 돌리지 않음
        if (!everCompleted && allPassedNow) {
            sessionManager.closeSession(session, avgScore, Map.of("avgScore", avgScore));
        } else if (!everCompleted) {
            sessionManager.updateStatus(session, "OPEN");
        }
        // everCompleted == true 인 경우는 상태 유지

        // 🔹 Review 세트 완주 시 Flow XP hook (PRACTICAL / REVIEW / rootTopicId)
        if (finalCompleted) {
            try {
                progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
                        req.userId(),
                        ExamMode.PRACTICAL.name(),
                        "REVIEW",
                        req.rootTopicId()
                ));
            } catch (Exception ignored) {
                // XP hook 실패는 학습 흐름을 막지 않음
            }
        }

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "REVIEW",
                "PRACTICAL_REVIEW_SET",
                finalCompleted ? "COMPLETE" : "IN_PROGRESS",
                finalCompleted ? "PRACTICAL_REVIEW_SUMMARY" : "PRACTICAL_REVIEW_SET",
                sessionManager.loadMeta(session),
                new PracticalDtos.PracticalReviewSubmitResp(
                        total,
                        total == 0 ? 0 : (int) Math.round(avgScore),
                        items,
                        wrongIds
                )
        );
    }

    /* ========================= 요약 (Micro Practical Summary) ========================= */

    @Transactional(readOnly = true)
    public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(String userId, Long topicId) {
        StudySession session = sessionManager.latestMicroSession(userId, topicId).orElse(null);

        int miniTotal = 0;
        int miniCorrect = 0;
        boolean miniPassed = false;

        int practicalTotal = 0;
        int practicalPassed = 0;
        double avgScore = 0.0;
        boolean practicalCompleted = false;

        List<String> mistakes = List.of();
        Map<String, Object> meta = Map.of();
        Long sessionId = null;

        if (session != null) {
            sessionId = session.getId();
            Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
            Map<String, Object> practicalMeta = sessionManager.loadStepMeta(session, "practical");

            miniTotal = readInt(miniMeta, "total");
            int miniWrong = readList(miniMeta, "wrongQuestionIds").size();
            miniCorrect = Math.max(0, miniTotal - miniWrong);
            miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));

            practicalTotal = readInt(practicalMeta, "total");
            int practicalWrong = readList(practicalMeta, "wrongQuestionIds").size();
            practicalPassed = Math.max(0, practicalTotal - practicalWrong);
            avgScore = readDouble(practicalMeta, "avgScore");
            practicalCompleted = Boolean.TRUE.equals(practicalMeta.get("completed"));

            meta = sessionManager.loadMeta(session);

            List<UserAnswer> sessionAnswers = userAnswerRepository.findByUserIdAndSessionId(userId, sessionId).stream()
                    .filter(ans -> ans.getExamMode() == ExamMode.PRACTICAL)
                    .toList();
            Set<Long> questionIds = sessionAnswers.stream().map(UserAnswer::getQuestionId).collect(Collectors.toSet());
            Map<Long, Question> questionCache = questionRepository.findByIdIn(questionIds).stream()
                    .filter(q -> Objects.equals(q.getTopicId(), topicId))
                    .collect(Collectors.toMap(Question::getId, q -> q));
            List<UserAnswer> topicAnswers = sessionAnswers.stream()
                    .filter(ans -> questionCache.containsKey(ans.getQuestionId()))
                    .toList();
            mistakes = collectMistakes(topicAnswers, questionCache);
        }

        boolean completed = miniPassed && practicalCompleted;
        int totalSolved = miniTotal + practicalTotal;
        int totalPassed = miniCorrect + practicalPassed;

        // 토픽 제목도 cert-service(커리큘럼)에서 가져오도록 수정
        String topicTitle = "";
        try {
            var curriculum = curriculumGateway.getConceptWithTopic(topicId);
            topicTitle = curriculum.topicTitle();
        } catch (Exception ignored) {
            // 커리큘럼 장애 시에도 요약은 진행
        }

        String summary = aiExplanationService.summarizePractical(
                topicTitle,
                totalSolved,
                (int) Math.round(avgScore),
                mistakes
        );

        WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
                miniTotal,
                miniCorrect,
                miniPassed,
                practicalTotal,
                practicalPassed,
                summary,
                completed
        );

        String status;
        if (session == null) {
            status = "NOT_STARTED";
        } else {
            status = completed ? "COMPLETE" : "IN_PROGRESS";
        }

        // 🔹 Practical Micro 세트 완주 시 Flow XP hook (PRACTICAL / MICRO / topicId)
        if (completed && sessionId != null) {
            try {
                progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
                        userId,
                        ExamMode.PRACTICAL.name(),
                        "MICRO",
                        topicId
                ));
            } catch (Exception ignored) {
                // XP hook 실패는 학습 흐름을 막지 않음
            }
        }

        return new FlowDtos.StepEnvelope<>(
                sessionId,
                "PRACTICAL",
                "PRACTICAL_SUMMARY",
                status,
                null,
                meta,
                payload
        );
    }

    public PracticalDtos.PracticalGradeOneResp gradeOnePractical(PracticalDtos.PracticalGradeOneReq req) {
        FlowDtos.StepEnvelope<PracticalDtos.PracticalSubmitResp> envelope = submitPractical(new PracticalDtos.PracticalSubmitReq(
                req.userId(),
                req.topicId(),
                List.of(new PracticalDtos.PracticalAnswer(req.questionId(), req.userText()))
        ));

        PracticalDtos.PracticalSubmitResp resp = envelope.payload();
        PracticalDtos.PracticalSubmitItem item = resp.items().isEmpty()
                ? new PracticalDtos.PracticalSubmitItem(req.questionId(), 0, "", "")
                : resp.items().get(0);

        return new PracticalDtos.PracticalGradeOneResp(item.score(), item.baseExplanation(), item.aiExplanation());
    }

    /* ========================= Wrong Recap (세션 기준) ========================= */

    @Transactional(readOnly = true)
    public WrongRecapDtos.WrongRecapSet wrongRecapBySession(String userId, Long sessionId, String stepCode) {
        StudySession session = sessionManager.getSession(sessionId);
        if (!session.getUserId().equals(userId)) {
            throw new IllegalStateException("세션 소유자가 아닙니다.");
        }

        // stepCode(MICRO_OX / PRACTICAL_SET / REVIEW ...) → UserAnswer.source 로 매핑
        String source = mapStepToSource(stepCode);

        // 이 사용자 + 해당 세션 + 해당 step(source)에서 틀린 답안만 수집
        List<UserAnswer> wrongAnswers = userAnswerRepository.findByUserId(userId).stream()
                .filter(ans -> Objects.equals(ans.getSessionId(), sessionId))
                .filter(ans -> ans.getExamMode() == ExamMode.PRACTICAL)
                .filter(ans -> Objects.equals(source, ans.getSource()))
                .filter(ans -> Boolean.FALSE.equals(ans.getCorrect()))
                .sorted(Comparator.comparing(UserAnswer::getAnsweredAt))
                .toList();

        if (wrongAnswers.isEmpty()) {
            return new WrongRecapDtos.WrongRecapSet(List.of());
        }

        // 문제 캐시
        LinkedHashSet<Long> qIds = wrongAnswers.stream()
                .map(UserAnswer::getQuestionId)
                .collect(Collectors.toCollection(LinkedHashSet::new));

        Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
                .filter(q -> q.getMode() == ExamMode.PRACTICAL)
                .collect(Collectors.toMap(Question::getId, q -> q));

        List<WrongRecapDtos.WrongRecapSet.Item> items = wrongAnswers.stream()
                .map(ans -> {
                    Question q = questionMap.get(ans.getQuestionId());
                    if (q == null) return null;
                    return buildWrongRecapItem(q, ans);
                })
                .filter(Objects::nonNull)
                .toList();

        return new WrongRecapDtos.WrongRecapSet(items);
    }

    /* ========================= Helper Methods ========================= */

    private boolean isPractical(Question question) {
        return question.getType() == QuestionType.SHORT || question.getType() == QuestionType.LONG;
    }

    private void persistUserAnswer(String userId,
                                   Question question,
                                   String answerText,
                                   boolean correct,
                                   int score,
                                   StudySession session,
                                   StudySessionItem item,
                                   String source) {
        UserAnswer userAnswer = UserAnswer.builder()
                .userId(userId)
                .questionId(question.getId())
                .examMode(ExamMode.PRACTICAL)
                .questionType(question.getType())
                .answeredAt(Instant.now())
                .userAnswerJson(toJson(Map.of(
                        "answer", Optional.ofNullable(answerText).orElse(""),
                        "score", score,
                        "passed", correct
                )))
                .correct(correct)
                .score(score)
                .source(source)
                .sessionId(session.getId())
                .sessionItemId(item.getId())
                .build();
        userAnswerRepository.save(userAnswer);
    }

    private void updateProgress(String userId, Long topicId, int score) {
        UserProgress progress = userProgressRepository.findByUserIdAndTopicId(userId, topicId)
                .orElseGet(() -> UserProgress.builder()
                        .userId(userId)
                        .topicId(topicId)
                        .writtenDoneCnt(0)
                        .practicalDoneCnt(0)
                        .writtenAccuracy(0.0)
                        .practicalAvgScore(0.0)
                        .updatedAt(Instant.now())
                        .build());

        int total = Optional.ofNullable(progress.getPracticalDoneCnt()).orElse(0);
        double avg = Optional.ofNullable(progress.getPracticalAvgScore()).orElse(0.0);
        progress.setPracticalDoneCnt(total + 1);
        double newAvg = ((avg * total) + score) / (total + 1);
        progress.setPracticalAvgScore(Math.round(newAvg * 10.0) / 10.0);
        progress.setLastStudiedAt(Instant.now());
        progress.setUpdatedAt(Instant.now());
        userProgressRepository.save(progress);
    }

    private void pushProgressHook(String userId, QuestionType type, boolean correct, int score, Long questionId) {
        List<String> tags = questionTagRepository.findTagsByQuestionId(questionId);
        ProgressHookClient.SubmitPayload payload = new ProgressHookClient.SubmitPayload(
                userId,
                ExamMode.PRACTICAL.name(),
                type.name(),
                correct,
                score,
                tags,
                "STUDY_SERVICE"
        );
        try {
            progressHookClient.submit(payload);
        } catch (Exception ignored) {
            // hook failure는 비차단
        }
    }

    private List<String> collectMistakes(List<UserAnswer> answers, Map<Long, Question> questionCache) {
        return answers.stream()
                .filter(ans -> Optional.ofNullable(ans.getScore()).orElse(0) < 60)
                .map(ans -> questionCache.get(ans.getQuestionId()))
                .filter(Objects::nonNull)
                .flatMap(q -> questionTagRepository.findTagsByQuestionId(q.getId()).stream())
                .distinct()
                .toList();
    }

    private String toJson(Object payload) {
        try {
            return objectMapper.writeValueAsString(payload);
        } catch (JsonProcessingException e) {
            return "{}";
        }
    }

    // 세션 기반 recap용: Question + UserAnswer 로 recap 아이템 구성
    private WrongRecapDtos.WrongRecapSet.Item buildWrongRecapItem(Question question, UserAnswer answer) {
        String stem = Optional.ofNullable(question.getStem()).orElse("");
        String baseExplain = Optional.ofNullable(question.getSolutionText()).orElse("");
        String userAnswerJson = (answer == null)
                ? "{}"
                : Optional.ofNullable(answer.getUserAnswerJson()).orElse("{}");

        // 실기는 정답 텍스트가 명확하지 않으므로 correctAnswer 는 비움("")
        return new WrongRecapDtos.WrongRecapSet.Item(
                question.getId(),
                question.getType().name(),
                stem,
                userAnswerJson,
                "",
                baseExplain,
                question.getImageUrl()
        );
    }

    private int readInt(Map<String, Object> meta, String key) {
        Object value = meta.get(key);
        if (value instanceof Number number) {
            return number.intValue();
        }
        if (value instanceof String str && !str.isBlank()) {
            try {
                return Integer.parseInt(str);
            } catch (NumberFormatException ignored) {
            }
        }
        return 0;
    }

    private double readDouble(Map<String, Object> meta, String key) {
        Object value = meta.get(key);
        if (value instanceof Number number) {
            return number.doubleValue();
        }
        if (value instanceof String str && !str.isBlank()) {
            try {
                return Double.parseDouble(str);
            } catch (NumberFormatException ignored) {
            }
        }
        return 0.0;
    }

    @SuppressWarnings("unchecked")
    private List<Long> readList(Map<String, Object> meta, String key) {
        Object value = meta.get(key);
        if (value instanceof List<?> list) {
            List<Long> result = new ArrayList<>();
            for (Object element : list) {
                if (element instanceof Number number) {
                    result.add(number.longValue());
                } else if (element instanceof String str && !str.isBlank()) {
                    try {
                        result.add(Long.parseLong(str));
                    } catch (NumberFormatException ignored) {
                    }
                }
            }
            return result;
        }
        return List.of();
    }

    // stepCode(MICRO_OX / PRACTICAL_SET / REVIEW ...) → UserAnswer.source 로 매핑
    private String mapStepToSource(String stepCode) {
        if (stepCode == null || stepCode.isBlank()) {
            // 기본은 실기 Micro 세트 기준
            return "MICRO_PRACTICAL";
        }
        return switch (stepCode) {
            case "MICRO_OX", "PRACTICAL_MINI" -> "PRACTICAL_MINI";              // 실기 OX
            case "PRACTICAL_SET", "MICRO_PRACTICAL" -> "MICRO_PRACTICAL";       // 실기 Micro 세트
            case "REVIEW", "PRACTICAL_REVIEW_SET", "PRACTICAL_REVIEW" -> "PRACTICAL_REVIEW"; // 실기 Review
            default -> stepCode; // 혹시 다른 source 를 그대로 사용하고 싶을 때
        };
    }
}
