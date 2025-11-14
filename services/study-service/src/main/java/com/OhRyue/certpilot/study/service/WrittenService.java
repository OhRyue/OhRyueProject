package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.domain.*;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.ReviewDtos;
import com.OhRyue.certpilot.study.dto.WrittenDtos;
import com.OhRyue.certpilot.study.dto.WrongRecapDtos;
import com.OhRyue.certpilot.study.repository.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class WrittenService {

    private static final int MINI_SIZE = 4;
    private static final int MCQ_SIZE = 5;
    private static final int REVIEW_SIZE = 20;

    private final TopicRepository topicRepository;
    private final ConceptRepository conceptRepository;
    private final QuestionRepository questionRepository;
    private final QuestionChoiceRepository choiceRepository;
    private final QuestionTagRepository questionTagRepository;
    private final UserAnswerRepository userAnswerRepository;
    private final UserProgressRepository userProgressRepository;
    private final StudySessionManager sessionManager;
    private final AIExplanationService aiExplanationService;
    private final TopicTreeService topicTreeService;
    private final ProgressHookClient progressHookClient;
    private final ObjectMapper objectMapper;

    /* ========================= 개념 ========================= */

    @Transactional(readOnly = true)
    public WrittenDtos.ConceptResp loadConcept(Long topicId) {
        Topic topic = topicRepository.findById(topicId)
                .orElseThrow(() -> new NoSuchElementException("Topic not found: %d".formatted(topicId)));
        Optional<Concept> conceptOpt = conceptRepository.findByTopicId(topicId);

        List<WrittenDtos.ConceptResp.Section> sections = conceptOpt
                .map(concept -> ConceptMapper.toSections(concept.getBlocksJson()))
                .orElse(List.of());

        return new WrittenDtos.ConceptResp(topicId, topic.getTitle(), sections);
    }

    /* ========================= 미니체크(OX) ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> miniSet(String userId, Long topicId) {
        List<Question> questions = questionRepository.pickRandomByTopic(
                topicId, ExamMode.WRITTEN, QuestionType.OX, PageRequest.of(0, MINI_SIZE));

        List<WrittenDtos.MiniQuestion> items = questions.stream()
                .map(q -> new WrittenDtos.MiniQuestion(q.getId(), Optional.ofNullable(q.getStem()).orElse("")))
                .toList();

        StudySession session = sessionManager.ensureMicroSession(
                userId, topicId, ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);
        Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
        boolean passed = Boolean.TRUE.equals(miniMeta.get("passed"));

        String status = passed ? "COMPLETE" : "IN_PROGRESS";
        String nextStep = passed ? "MICRO_MCQ" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "MICRO",
                "MICRO_MINI",
                status,
                nextStep,
                sessionManager.loadMeta(session),
                new WrittenDtos.MiniSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(WrittenDtos.MiniSubmitReq req) {
        Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
                .map(WrittenDtos.MiniAnswer::questionId).toList(), QuestionType.OX);

        StudySession session = sessionManager.ensureMicroSession(
                req.userId(), req.topicId(), ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);
        int baseOrder = sessionManager.items(session.getId()).size();

        int correctCount = 0;
        List<WrittenDtos.MiniSubmitItem> resultItems = new ArrayList<>();
        List<Long> wrongQuestionIds = new ArrayList<>();

        for (int idx = 0; idx < req.answers().size(); idx++) {
            WrittenDtos.MiniAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null) {
                throw new NoSuchElementException("Question not found: " + answer.questionId());
            }

            String correctAnswer = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
            String userAnswer = Boolean.TRUE.equals(answer.answer()) ? "O" : "X";
            boolean isCorrect = correctAnswer.equalsIgnoreCase(userAnswer);
            if (isCorrect) correctCount++; else wrongQuestionIds.add(question.getId());

            String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");
            resultItems.add(new WrittenDtos.MiniSubmitItem(question.getId(), isCorrect, explanation, ""));

            String answerJson = toJson(Map.of(
                    "answer", userAnswer,
                    "correct", isCorrect,
                    "submittedAt", Instant.now().toString()
            ));

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    answerJson,
                    isCorrect,
                    isCorrect ? 100 : 0,
                    null
            );

            persistUserAnswer(req.userId(), question, userAnswer, isCorrect, 100, session, item, "MICRO_MINI");
            pushProgressHook(req.userId(), ExamMode.WRITTEN, QuestionType.OX, isCorrect, 100, question.getId());
            updateProgress(req.userId(), question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);
        }

        boolean passed = correctCount == req.answers().size();
        Map<String, Object> miniMeta = new HashMap<>();
        miniMeta.put("total", req.answers().size());
        miniMeta.put("correct", correctCount);
        miniMeta.put("passed", passed);
        miniMeta.put("wrongQuestionIds", wrongQuestionIds);
        miniMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "mini", miniMeta);

        String status = passed ? "COMPLETE" : "IN_PROGRESS";
        String nextStep = passed ? "MICRO_MCQ" : "MICRO_MINI";

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "MICRO",
                "MICRO_MINI",
                status,
                nextStep,
                sessionManager.loadMeta(session),
                new WrittenDtos.MiniSubmitResp(req.answers().size(), correctCount, passed, resultItems, wrongQuestionIds)
        );
    }

    /* ========================= MCQ ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.McqSet> mcqSet(Long topicId, String userId) {
        StudySession session = sessionManager.ensureMicroSession(
                userId, topicId, ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);

        Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
        boolean miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
        if (!miniPassed) {
            throw new IllegalStateException("MINI_STEP_NOT_PASSED");
        }

        Map<String, Object> mcqMeta = sessionManager.loadStepMeta(session, "mcq");
        boolean completed = Boolean.TRUE.equals(mcqMeta.get("completed"));

        List<Question> questions = questionRepository.pickRandomByTopic(
                topicId, ExamMode.WRITTEN, QuestionType.MCQ, PageRequest.of(0, MCQ_SIZE));

        List<WrittenDtos.McqQuestion> items = questions.stream()
                .map(q -> new WrittenDtos.McqQuestion(
                        q.getId(),
                        Optional.ofNullable(q.getStem()).orElse(""),
                        loadChoices(q.getId()),
                        q.getImageUrl()
                ))
                .toList();

        String status = completed ? "COMPLETE" : "IN_PROGRESS";
        String nextStep = completed ? "MICRO_SUMMARY" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "MICRO",
                "MICRO_MCQ",
                status,
                nextStep,
                sessionManager.loadMeta(session),
                new WrittenDtos.McqSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> submitMcq(WrittenDtos.McqSubmitReq req) {
        Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
                .map(WrittenDtos.McqAnswer::questionId).toList(), QuestionType.MCQ);

        StudySession session = sessionManager.ensureMicroSession(
                req.userId(), req.topicId(), ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);
        Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
        if (!Boolean.TRUE.equals(miniMeta.get("passed"))) {
            throw new IllegalStateException("MINI_STEP_NOT_PASSED");
        }
        int baseOrder = sessionManager.items(session.getId()).size();

        int correctCount = 0;
        List<WrittenDtos.McqSubmitItem> items = new ArrayList<>();
        List<Long> wrongIds = new ArrayList<>();

        for (int idx = 0; idx < req.answers().size(); idx++) {
            WrittenDtos.McqAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null) throw new NoSuchElementException("Question not found: " + answer.questionId());

            String correctLabel = resolveCorrectChoice(question.getId());
            boolean isCorrect = Objects.equals(correctLabel, answer.label());
            if (isCorrect) correctCount++; else wrongIds.add(question.getId());

            String dbExplanation = Optional.ofNullable(question.getSolutionText()).orElse("");
            String aiExplanation = isCorrect ? "" :
                    aiExplanationService.explainWrongForMCQ(answer.label(), correctLabel, question);

            items.add(new WrittenDtos.McqSubmitItem(
                    question.getId(),
                    isCorrect,
                    correctLabel,
                    dbExplanation,
                    aiExplanation
            ));

            Map<String, Object> answerPayload = new HashMap<>();
            answerPayload.put("answer", answer.label());
            answerPayload.put("correctLabel", correctLabel);
            answerPayload.put("correct", isCorrect);
            answerPayload.put("submittedAt", Instant.now().toString());
            if (!aiExplanation.isBlank()) answerPayload.put("aiExplain", aiExplanation);

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    toJson(answerPayload),
                    isCorrect,
                    isCorrect ? 100 : 0,
                    aiExplanation.isBlank() ? null : toJson(Map.of("explain", aiExplanation))
            );

            persistUserAnswer(req.userId(), question, answer.label(), isCorrect, 100, session, item, "MICRO_MCQ");
            pushProgressHook(req.userId(), ExamMode.WRITTEN, QuestionType.MCQ, isCorrect, 100, question.getId());
            updateProgress(req.userId(), question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);
        }

        boolean allCorrect = !items.isEmpty() && wrongIds.isEmpty();
        double scorePct = items.isEmpty() ? 0.0 : (correctCount * 100.0) / items.size();

        Map<String, Object> mcqMeta = new HashMap<>();
        mcqMeta.put("total", req.answers().size());
        mcqMeta.put("correct", correctCount);
        mcqMeta.put("completed", allCorrect);
        mcqMeta.put("scorePct", scorePct);
        mcqMeta.put("wrongQuestionIds", wrongIds);
        mcqMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "mcq", mcqMeta);

        if (allCorrect) {
            sessionManager.closeSession(session, scorePct, Map.of("finalScorePct", scorePct));
        } else {
            sessionManager.updateStatus(session, "OPEN");
        }

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "MICRO",
                "MICRO_MCQ",
                allCorrect ? "COMPLETE" : "IN_PROGRESS",
                allCorrect ? "MICRO_SUMMARY" : "MICRO_MCQ",
                sessionManager.loadMeta(session),
                new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds)
        );
    }

    /* ========================= 리뷰 ========================= */

    @Transactional
    public FlowDtos.StepEnvelope<ReviewDtos.ReviewSet> reviewSet(String userId, Long rootTopicId) {
        Set<Long> topicIds = topicTreeService.descendantIds(rootTopicId);
        if (topicIds.isEmpty()) topicIds = Set.of(rootTopicId);

        List<Question> questions = questionRepository.pickRandomByTopicIn(
                topicIds, ExamMode.WRITTEN, QuestionType.MCQ, PageRequest.of(0, REVIEW_SIZE));

        List<ReviewDtos.ReviewQuestion> items = questions.stream()
                .map(q -> new ReviewDtos.ReviewQuestion(
                        q.getId(),
                        Optional.ofNullable(q.getStem()).orElse(""),
                        loadReviewChoices(q.getId()),
                        q.getImageUrl()
                )).toList();

        StudySession session = sessionManager.ensureReviewSession(
                userId, rootTopicId, ExamMode.WRITTEN, REVIEW_SIZE);
        Map<String, Object> reviewMeta = sessionManager.loadStepMeta(session, "review");
        boolean completed = Boolean.TRUE.equals(reviewMeta.get("completed"));

        String status = completed ? "COMPLETE" : "IN_PROGRESS";
        String nextStep = completed ? "REVIEW_SUMMARY" : null;

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "REVIEW",
                "REVIEW_SET",
                status,
                nextStep,
                sessionManager.loadMeta(session),
                new ReviewDtos.ReviewSet(items)
        );
    }

    @Transactional
    public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> reviewSubmitWritten(
            WrittenDtos.McqSubmitReq req, Long rootTopicId) {

        Set<Long> rawIds = topicTreeService.descendantIds(rootTopicId);
        Set<Long> topicIds = new HashSet<>(rawIds);
        if (topicIds.isEmpty()) {
            topicIds.add(rootTopicId);
        }
        final Set<Long> targetTopicIds = Set.copyOf(topicIds);

        Map<Long, Question> questionMap = questionRepository.findByIdIn(
                        req.answers().stream().map(WrittenDtos.McqAnswer::questionId).toList())
                .stream()
                .filter(q -> q.getMode() == ExamMode.WRITTEN && targetTopicIds.contains(q.getTopicId()))
                .collect(Collectors.toMap(Question::getId, q -> q));

        StudySession session = sessionManager.ensureReviewSession(
                req.userId(), rootTopicId, ExamMode.WRITTEN, REVIEW_SIZE);
        int baseOrder = sessionManager.items(session.getId()).size();

        int correctCount = 0;
        List<WrittenDtos.McqSubmitItem> items = new ArrayList<>();
        List<Long> wrongIds = new ArrayList<>();

        for (int idx = 0; idx < req.answers().size(); idx++) {
            WrittenDtos.McqAnswer answer = req.answers().get(idx);
            Question question = questionMap.get(answer.questionId());
            if (question == null) continue;

            String correctLabel = resolveCorrectChoice(question.getId());
            boolean isCorrect = Objects.equals(correctLabel, answer.label());
            if (isCorrect) correctCount++; else wrongIds.add(question.getId());

            String dbExplanation = Optional.ofNullable(question.getSolutionText()).orElse("");
            String aiExplanation = isCorrect ? "" :
                    aiExplanationService.explainWrongForMCQ(answer.label(), correctLabel, question);

            items.add(new WrittenDtos.McqSubmitItem(
                    question.getId(),
                    isCorrect,
                    correctLabel,
                    dbExplanation,
                    aiExplanation
            ));

            Map<String, Object> answerPayload = new HashMap<>();
            answerPayload.put("answer", answer.label());
            answerPayload.put("correctLabel", correctLabel);
            answerPayload.put("correct", isCorrect);
            answerPayload.put("submittedAt", Instant.now().toString());
            if (!aiExplanation.isBlank()) answerPayload.put("aiExplain", aiExplanation);

            StudySessionItem item = sessionManager.upsertItem(
                    session,
                    question.getId(),
                    baseOrder + idx + 1,
                    toJson(answerPayload),
                    isCorrect,
                    isCorrect ? 100 : 0,
                    aiExplanation.isBlank() ? null : toJson(Map.of("explain", aiExplanation))
            );

            persistUserAnswer(req.userId(), question, answer.label(), isCorrect, 100, session, item, "REVIEW_MCQ");
        }

        boolean allCorrect = !items.isEmpty() && wrongIds.isEmpty();

        Map<String, Object> reviewMeta = new HashMap<>();
        reviewMeta.put("total", req.answers().size());
        reviewMeta.put("correct", correctCount);
        reviewMeta.put("completed", allCorrect);
        reviewMeta.put("wrongQuestionIds", wrongIds);
        reviewMeta.put("lastSubmittedAt", Instant.now().toString());
        sessionManager.saveStepMeta(session, "review", reviewMeta);

        if (allCorrect) {
            double scorePct = req.answers().isEmpty() ? 0.0 : (correctCount * 100.0) / req.answers().size();
            sessionManager.closeSession(session, scorePct, Map.of("reviewScorePct", scorePct));
        } else {
            sessionManager.updateStatus(session, "OPEN");
        }

        return new FlowDtos.StepEnvelope<>(
                session.getId(),
                "REVIEW",
                "REVIEW_SET",
                allCorrect ? "COMPLETE" : "IN_PROGRESS",
                allCorrect ? "REVIEW_SUMMARY" : "REVIEW_SET",
                sessionManager.loadMeta(session),
                new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds)
        );
    }

    /* ========================= 요약 ========================= */

    @Transactional(readOnly = true)
    public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(String userId, Long topicId) {
        StudySession session = sessionManager.latestMicroSession(userId, topicId).orElse(null);

        int miniTotal = 0;
        int miniCorrect = 0;
        boolean miniPassed = false;

        int mcqTotal = 0;
        int mcqCorrect = 0;
        boolean mcqCompleted = false;

        List<String> weakTags = List.of();
        Map<String, Object> meta = Map.of();
        Long sessionId = null;

        if (session != null) {
            sessionId = session.getId();
            Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
            Map<String, Object> mcqMeta = sessionManager.loadStepMeta(session, "mcq");

            miniTotal = readInt(miniMeta, "total");
            miniCorrect = readInt(miniMeta, "correct");
            miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));

            mcqTotal = readInt(mcqMeta, "total");
            mcqCorrect = readInt(mcqMeta, "correct");
            mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));

            meta = sessionManager.loadMeta(session);

            List<UserAnswer> sessionAnswers = userAnswerRepository.findByUserIdAndSessionId(userId, sessionId).stream()
                    .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
                    .toList();
            Set<Long> questionIds = sessionAnswers.stream().map(UserAnswer::getQuestionId).collect(Collectors.toSet());
            Map<Long, Question> questionCache = questionRepository.findByIdIn(questionIds).stream()
                    .filter(q -> Objects.equals(q.getTopicId(), topicId))
                    .collect(Collectors.toMap(Question::getId, q -> q));
            List<UserAnswer> answers = sessionAnswers.stream()
                    .filter(ans -> questionCache.containsKey(ans.getQuestionId()))
                    .toList();
            weakTags = computeWeakTags(answers, questionCache);
        }

        int totalSolved = miniTotal + mcqTotal;
        int totalCorrect = miniCorrect + mcqCorrect;
        boolean completed = miniPassed && mcqCompleted;

        String summary = aiExplanationService.summarizeWritten(
                topicRepository.findById(topicId).map(Topic::getTitle).orElse(""),
                totalSolved,
                totalCorrect,
                weakTags
        );

        WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
                miniTotal,
                miniCorrect,
                miniPassed,
                mcqTotal,
                mcqCorrect,
                summary,
                completed
        );

        String status;
        if (session == null) {
            status = "NOT_STARTED";
        } else {
            status = completed ? "COMPLETE" : "IN_PROGRESS";
        }

        return new FlowDtos.StepEnvelope<>(
                sessionId,
                "MICRO",
                "MICRO_SUMMARY",
                status,
                null,
                meta,
                payload
        );
    }

    @Transactional(readOnly = true)
    public WrongRecapDtos.WrongRecapSet wrongRecapBySession(String userId, Long sessionId, String stepCode) {
        StudySession session = sessionManager.getSession(sessionId);
        if (!session.getUserId().equals(userId)) {
            throw new IllegalStateException("세션 소유자가 아닙니다.");
        }

        String stepKey = mapStepKey(stepCode);
        List<Long> questionIds = sessionManager.wrongQuestionIds(sessionId, stepKey);
        if (questionIds.isEmpty()) {
            return new WrongRecapDtos.WrongRecapSet(List.of());
        }

        Map<Long, Question> questionCache = questionRepository.findAllById(questionIds).stream()
                .filter(q -> q.getMode() == session.getExamMode())
                .collect(Collectors.toMap(Question::getId, q -> q));
        Map<Long, UserAnswer> latestAnswers = latestAnswerMap(userId);

        List<WrongRecapDtos.WrongRecapSet.Item> items = questionIds.stream()
                .map(questionCache::get)
                .filter(Objects::nonNull)
                .map(question -> toWrongRecapItem(question, latestAnswers))
                .toList();
        return new WrongRecapDtos.WrongRecapSet(items);
    }

    /* ========================= Wrong Recap ========================= */

    @Transactional(readOnly = true)
    public WrongRecapDtos.WrongRecapSet wrongRecap(Long topicId, String userId, int limit) {
        List<UserAnswer> wrongAnswers = userAnswerRepository.findByUserId(userId).stream()
                .filter(ans -> Boolean.FALSE.equals(ans.getCorrect()))
                .sorted(Comparator.comparing(UserAnswer::getAnsweredAt).reversed())
                .toList();

        Set<Long> answerQuestionIds = wrongAnswers.stream()
                .map(UserAnswer::getQuestionId)
                .collect(Collectors.toSet());
        Map<Long, Question> questionCache = questionRepository.findByIdIn(answerQuestionIds).stream()
                .filter(q -> Objects.equals(q.getTopicId(), topicId))
                .collect(Collectors.toMap(Question::getId, q -> q));
        Map<Long, UserAnswer> latestAnswers = latestAnswerMap(userId);

        LinkedHashSet<Long> questionIds = new LinkedHashSet<>();
        for (UserAnswer ans : wrongAnswers) {
            if (questionCache.containsKey(ans.getQuestionId())) {
                questionIds.add(ans.getQuestionId());
                if (questionIds.size() >= Math.max(limit, 50)) break;
            }
        }

        List<WrongRecapDtos.WrongRecapSet.Item> items = questionIds.stream()
                .map(questionCache::get)
                .filter(Objects::nonNull)
                .map(question -> toWrongRecapItem(question, latestAnswers))
                .limit(limit)
                .toList();

        return new WrongRecapDtos.WrongRecapSet(items);
    }

    @Transactional(readOnly = true)
    public WrongRecapDtos.WrongRecapSet wrongRecapByIds(String ids, String userId) {
        List<Long> questionIds = Arrays.stream(ids.split(","))
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .map(Long::valueOf)
                .distinct()
                .toList();

        Map<Long, UserAnswer> latestAnswers = latestAnswerMap(userId);
        List<WrongRecapDtos.WrongRecapSet.Item> items = questionRepository.findAllById(questionIds).stream()
                .map(question -> toWrongRecapItem(question, latestAnswers))
                .toList();

        return new WrongRecapDtos.WrongRecapSet(items);
    }

    /* ========================= 즉시 채점 ========================= */

    public WrittenDtos.MiniGradeOneResp gradeOneMini(WrittenDtos.MiniGradeOneReq req) {
        FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> envelope = submitMini(new WrittenDtos.MiniSubmitReq(
                req.userId(),
                req.topicId(),
                List.of(new WrittenDtos.MiniAnswer(req.questionId(), req.answer()))
        ));

        WrittenDtos.MiniSubmitResp resp = envelope.payload();
        WrittenDtos.MiniSubmitItem item = resp.items().isEmpty()
                ? new WrittenDtos.MiniSubmitItem(req.questionId(), false, "", "")
                : resp.items().get(0);

        return new WrittenDtos.MiniGradeOneResp(item.correct(), item.explanation());
    }

    public WrittenDtos.McqGradeOneResp gradeOneMcq(WrittenDtos.McqGradeOneReq req) {
        FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> envelope = submitMcq(new WrittenDtos.McqSubmitReq(
                req.userId(),
                req.topicId(),
                List.of(new WrittenDtos.McqAnswer(req.questionId(), req.label()))
        ));

        WrittenDtos.McqSubmitResp resp = envelope.payload();
        WrittenDtos.McqSubmitItem item = resp.items().isEmpty()
                ? new WrittenDtos.McqSubmitItem(req.questionId(), false, "", "", "")
                : resp.items().get(0);

        return new WrittenDtos.McqGradeOneResp(
                item.correct(),
                item.correctLabel(),
                item.explanation(),
                item.aiExplanation()
        );
    }

    /* ========================= 내부 유틸 ========================= */

    private Map<Long, Question> fetchQuestions(List<Long> ids, QuestionType expectedType) {
        List<Question> questions = questionRepository.findByIdIn(ids);
        return questions.stream()
                .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == expectedType)
                .collect(Collectors.toMap(Question::getId, q -> q));
    }

    private List<WrittenDtos.McqChoice> loadChoices(Long questionId) {
        return choiceRepository.findByQuestionId(questionId).stream()
                .sorted(Comparator.comparing(QuestionChoice::getLabel))
                .map(choice -> new WrittenDtos.McqChoice(choice.getLabel(), choice.getContent()))
                .toList();
    }

    private List<ReviewDtos.ReviewQuestion.Choice> loadReviewChoices(Long questionId) {
        return choiceRepository.findByQuestionId(questionId).stream()
                .sorted(Comparator.comparing(QuestionChoice::getLabel))
                .map(choice -> new ReviewDtos.ReviewQuestion.Choice(choice.getLabel(), choice.getContent()))
                .toList();
    }

    private String resolveCorrectChoice(Long questionId) {
        return choiceRepository.findByQuestionId(questionId).stream()
                .filter(QuestionChoice::isCorrect)
                .map(QuestionChoice::getLabel)
                .findFirst()
                .orElse("");
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
                .examMode(question.getMode())
                .questionType(question.getType())
                .answeredAt(Instant.now())
                .userAnswerJson(toJson(Map.of("answer", answerText, "correct", correct, "score", score)))
                .correct(correct)
                .score(score)
                .source(source)
                .sessionId(session.getId())
                .sessionItemId(item.getId())
                .build();
        userAnswerRepository.save(userAnswer);
    }

    private void updateProgress(String userId, Long topicId, ExamMode mode, boolean correct, int score) {
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

        if (mode == ExamMode.WRITTEN) {
            int total = Optional.ofNullable(progress.getWrittenDoneCnt()).orElse(0);
            double acc = Optional.ofNullable(progress.getWrittenAccuracy()).orElse(0.0);
            progress.setWrittenDoneCnt(total + 1);
            double newAcc = ((acc * total) + (correct ? 100 : 0)) / (total + 1);
            progress.setWrittenAccuracy(Math.round(newAcc * 10.0) / 10.0);
        } else {
            int total = Optional.ofNullable(progress.getPracticalDoneCnt()).orElse(0);
            double avg = Optional.ofNullable(progress.getPracticalAvgScore()).orElse(0.0);
            progress.setPracticalDoneCnt(total + 1);
            double newAvg = ((avg * total) + score) / (total + 1);
            progress.setPracticalAvgScore(Math.round(newAvg * 10.0) / 10.0);
        }
        progress.setLastStudiedAt(Instant.now());
        progress.setUpdatedAt(Instant.now());
        userProgressRepository.save(progress);
    }

    private void pushProgressHook(String userId, ExamMode mode, QuestionType type, boolean correct, int score, Long questionId) {
        List<String> tags = questionTagRepository.findTagsByQuestionId(questionId);
        ProgressHookClient.SubmitPayload payload = new ProgressHookClient.SubmitPayload(
                userId,
                mode.name(),
                type.name(),
                correct,
                score,
                tags,
                "STUDY_SERVICE"
        );
        try {
            progressHookClient.submit(payload);
        } catch (Exception ignored) {
            // hook failure is non-blocking
        }
    }

    private WrongRecapDtos.WrongRecapSet.Item toWrongRecapItem(Question question, Map<Long, UserAnswer> latestAnswers) {
        String stem = Optional.ofNullable(question.getStem()).orElse("");
        String solution = Optional.ofNullable(question.getSolutionText()).orElse("");
        String correctAnswer = switch (question.getType()) {
            case OX -> Optional.ofNullable(question.getAnswerKey()).orElse("");
            case MCQ -> resolveCorrectChoice(question.getId());
            default -> "";
        };

        String userAnswer = Optional.ofNullable(latestAnswers.get(question.getId()))
                .map(ans -> Optional.ofNullable(ans.getUserAnswerJson()).orElse("{}"))
                .orElse("{}");

        return new WrongRecapDtos.WrongRecapSet.Item(
                question.getId(),
                question.getType().name(),
                stem,
                userAnswer,
                correctAnswer,
                solution,
                question.getImageUrl()
        );
    }

    private List<String> computeWeakTags(List<UserAnswer> answers, Map<Long, Question> questionCache) {
        Map<Long, List<String>> tagCache = new HashMap<>();
        Map<String, int[]> stats = new HashMap<>();

        for (UserAnswer answer : answers) {
            Question question = questionCache.get(answer.getQuestionId());
            if (question == null) continue;
            List<String> tags = tagCache.computeIfAbsent(question.getId(),
                    id -> questionTagRepository.findTagsByQuestionId(id));
            for (String tag : tags) {
                int[] values = stats.computeIfAbsent(tag, t -> new int[2]);
                values[0] += 1;
                if (Boolean.TRUE.equals(answer.getCorrect())) values[1] += 1;
            }
        }

        return stats.entrySet().stream()
                .filter(e -> e.getValue()[0] >= 3) // 최소 시도 3회
                .filter(e -> e.getValue()[1] * 1.0 / e.getValue()[0] < 0.7)
                .map(Map.Entry::getKey)
                .sorted()
                .toList();
    }

    private String toJson(Map<String, Object> payload) {
        try {
            return objectMapper.writeValueAsString(payload);
        } catch (JsonProcessingException e) {
            return "{}";
        }
    }

    private Map<Long, UserAnswer> latestAnswerMap(String userId) {
        return userAnswerRepository.findByUserId(userId).stream()
                .collect(Collectors.groupingBy(
                        UserAnswer::getQuestionId,
                        Collectors.collectingAndThen(
                                Collectors.maxBy(Comparator.comparing(UserAnswer::getAnsweredAt)),
                                opt -> opt.orElse(null)
                        )
                ));
    }

    private static class ConceptMapper {
        private static final ObjectMapper mapper = new ObjectMapper();

        private static List<WrittenDtos.ConceptResp.Section> toSections(String json) {
            if (json == null || json.isBlank()) return List.of();
            try {
                var root = mapper.readTree(json);
                var sectionsNode = root.path("sections");
                if (!sectionsNode.isArray()) return List.of();
                List<WrittenDtos.ConceptResp.Section> sections = new ArrayList<>();
                sectionsNode.forEach(node -> sections.add(new WrittenDtos.ConceptResp.Section(
                        node.path("orderNo").asInt(),
                        node.path("subCode").asText(""),
                        node.path("title").asText(""),
                        node.path("importance").asInt(0),
                        toBlocks(node.path("blocks"))
                )));
                sections.sort(Comparator.comparing(WrittenDtos.ConceptResp.Section::orderNo));
                return sections;
            } catch (Exception e) {
                return List.of();
            }
        }

        private static List<WrittenDtos.ConceptResp.Block> toBlocks(com.fasterxml.jackson.databind.JsonNode blocksNode) {
            if (!blocksNode.isArray()) return List.of();
            List<WrittenDtos.ConceptResp.Block> blocks = new ArrayList<>();
            blocksNode.forEach(block -> blocks.add(new WrittenDtos.ConceptResp.Block(
                    block.path("type").asText(null),
                    block.path("text").asText(null),
                    toList(block.path("items")),
                    block.path("url").asText(null),
                    block.path("alt").asText(null),
                    block.path("caption").asText(null),
                    toList(block.path("headers")),
                    toMatrix(block.path("rows"))
            )));
            return blocks;
        }

        private static List<String> toList(com.fasterxml.jackson.databind.JsonNode node) {
            if (!node.isArray()) return List.of();
            List<String> list = new ArrayList<>();
            node.forEach(n -> list.add(n.asText()));
            return list;
        }

        private static List<List<String>> toMatrix(com.fasterxml.jackson.databind.JsonNode node) {
            if (!node.isArray()) return List.of();
            List<List<String>> rows = new ArrayList<>();
            node.forEach(row -> {
                List<String> cols = new ArrayList<>();
                row.forEach(col -> cols.add(col.asText()));
                rows.add(cols);
            });
            return rows;
        }
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

    private String mapStepKey(String stepCode) {
        if (stepCode == null || stepCode.isBlank()) {
            return "mcq";
        }
        return switch (stepCode) {
            case "MICRO_MINI" -> "mini";
            case "MICRO_MCQ" -> "mcq";
            case "REVIEW_SET", "REVIEW_MCQ" -> "review";
            default -> "mcq";
        };
    }
}
