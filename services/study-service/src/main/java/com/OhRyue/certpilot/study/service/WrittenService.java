package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.domain.*;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
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
  private static final int REVIEW_SIZE = 10;

  private final QuestionRepository questionRepository;
  private final QuestionChoiceRepository choiceRepository;
  private final QuestionTagRepository questionTagRepository;
  private final UserAnswerRepository userAnswerRepository;
  private final UserProgressRepository userProgressRepository;
  private final StudySessionManager sessionManager;
  private final LearningSessionService learningSessionService;
  private final AIExplanationService aiExplanationService;
  private final TopicTreeService topicTreeService;
  private final ProgressHookClient progressHookClient;
  private final ObjectMapper objectMapper;
  private final CurriculumGateway curriculumGateway;

  /* ========================= 개념 ========================= */

  @Transactional(readOnly = true)
  public WrittenDtos.ConceptResp loadConcept(Long topicId) {

    CurriculumGateway.CurriculumConcept concept =
        curriculumGateway.getConceptWithTopic(topicId);

    List<WrittenDtos.ConceptResp.Section> sections =
        ConceptMapper.toSections(concept.sectionsJson());

    return new WrittenDtos.ConceptResp(
        concept.topicId(),
        concept.topicTitle(),
        sections
    );
  }

  /* ========================= 미니체크(OX) ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> miniSet(Long topicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. 문제 로드 (세션 생성 없이 문제만 반환)
    List<Question> questions = questionRepository.pickRandomByTopic(
        topicId, ExamMode.WRITTEN, QuestionType.OX, PageRequest.of(0, MINI_SIZE));

    List<WrittenDtos.MiniQuestion> items = questions.stream()
        .map(q -> new WrittenDtos.MiniQuestion(q.getId(), Optional.ofNullable(q.getStem()).orElse("")))
        .toList();

    // 2. 기존 세션이 있으면 상태 확인 (없어도 문제는 반환)
    Long sessionId = null;
    String status = "IN_PROGRESS";
    String nextStep = null;
    Map<String, Object> meta = Map.of();

    Optional<LearningSession> existingSession = learningSessionService.findLearningSession(
        userId, topicId, ExamMode.WRITTEN);
    
    if (existingSession.isPresent()) {
      LearningSession learningSession = existingSession.get();
      try {
        LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
        boolean passed = "COMPLETE".equals(miniStep.getStatus());
        status = passed ? "COMPLETE" : "IN_PROGRESS";
        nextStep = passed ? "MCQ" : null;
        
        StudySession studySession = miniStep.getStudySession();
        if (studySession != null) {
          sessionId = studySession.getId();
          meta = sessionManager.loadMeta(studySession);
        }
      } catch (Exception e) {
        // LearningStep이 없거나 오류 발생 시 기본값 유지
      }
    }

    Long learningSessionId = existingSession.map(LearningSession::getId).orElse(null);
    
    return new FlowDtos.StepEnvelope<>(
        sessionId,
        "MICRO",
        "MICRO_MINI",
        status,
        nextStep,
        meta,
        new WrittenDtos.MiniSet(items),
        learningSessionId
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(WrittenDtos.MiniSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession과 MINI 단계 조회
    LearningSession learningSession = learningSessionService.getOrCreateLearningSession(
        userId, req.topicId(), ExamMode.WRITTEN);
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    
    // 2. StudySession 조회 또는 생성
    StudySession session = sessionManager.ensureStudySessionForStep(
        miniStep, userId, req.topicId(), ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);
    
    Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
        .map(WrittenDtos.MiniAnswer::questionId).toList(), QuestionType.OX);
    
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
      if (isCorrect) correctCount++;
      else wrongQuestionIds.add(question.getId());

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

      persistUserAnswer(userId, question, userAnswer, isCorrect, 100, session, item, "MICRO_MINI");
      pushProgressHook(userId, ExamMode.WRITTEN, QuestionType.OX, isCorrect, 100, question.getId());
      updateProgress(userId, question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);
    }

    boolean passedNow = correctCount == req.answers().size();
    int scorePct = req.answers().isEmpty() ? 0 : (correctCount * 100) / req.answers().size();

    // 3. LearningStep 업데이트 (이전 메타데이터 불러와서 누적)
    Map<String, Object> prevMiniMeta = parseJson(miniStep.getMetadataJson());
    Map<String, Object> miniMeta = new HashMap<>(prevMiniMeta);
    
    // 누적 로직
    int prevTotal = readInt(prevMiniMeta, "total");
    int prevCorrect = readInt(prevMiniMeta, "correct");
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds = prevMiniMeta.get("wrongQuestionIds") instanceof List<?> 
        ? (List<Long>) prevMiniMeta.get("wrongQuestionIds")
        : new ArrayList<>();
    
    int newTotal = prevTotal + req.answers().size();
    int newCorrect = prevCorrect + correctCount;
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    allWrongIds.addAll(wrongQuestionIds);
    boolean everPassed = Boolean.TRUE.equals(prevMiniMeta.get("passed")) || passedNow;
    
    miniMeta.put("total", newTotal);
    miniMeta.put("correct", newCorrect);
    miniMeta.put("passed", everPassed);
    miniMeta.put("wrongQuestionIds", allWrongIds);
    miniMeta.put("lastSubmittedAt", Instant.now().toString());
    
    // 누적된 값으로 scorePct 재계산
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;
    
    String metadataJson = toJson(miniMeta);
    learningSessionService.updateStepStatus(miniStep, "COMPLETE", accumulatedScorePct, metadataJson);

    // 4. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mini", miniMeta);

    // 5. 미니체크 4문제 완료 시 세션 종료
    if (newTotal >= MINI_SIZE) {
      sessionManager.closeSession(session, accumulatedScorePct, Map.of("miniScorePct", accumulatedScorePct));
    }

    String status = "COMPLETE";
    String nextStep = "MCQ";
    
    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "MICRO",
        "MICRO_MINI",
        status,
        nextStep,
        sessionManager.loadMeta(session),
        new WrittenDtos.MiniSubmitResp(req.answers().size(), correctCount, everPassed, resultItems, wrongQuestionIds),
        learningSession.getId()
    );
  }

  /* ========================= MCQ ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.McqSet> mcqSet(Long topicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession과 MCQ 단계 조회
    LearningSession learningSession = learningSessionService.getOrCreateLearningSession(
        userId, topicId, ExamMode.WRITTEN);
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");

    // 2. 해당 단계의 StudySession 조회 또는 생성
    StudySession studySession = sessionManager.ensureStudySessionForStep(
        mcqStep, userId, topicId, ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);

    // 3. 문제 로드
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

    // 4. 단계 상태 확인
    boolean completed = "COMPLETE".equals(mcqStep.getStatus());
    String status = completed ? "COMPLETE" : "IN_PROGRESS";
    String nextStep = completed ? "SUMMARY" : null;

    // 5. 메타데이터 로드
    Map<String, Object> meta = sessionManager.loadMeta(studySession);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "MICRO",
        "MICRO_MCQ",
        status,
        nextStep,
        meta,
        new WrittenDtos.McqSet(items),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> submitMcq(WrittenDtos.McqSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession과 MCQ 단계 조회
    LearningSession learningSession = learningSessionService.getOrCreateLearningSession(
        userId, req.topicId(), ExamMode.WRITTEN);
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // 2. StudySession 조회 또는 생성
    StudySession session = sessionManager.ensureStudySessionForStep(
        mcqStep, userId, req.topicId(), ExamMode.WRITTEN, MINI_SIZE + MCQ_SIZE);

    Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
        .map(WrittenDtos.McqAnswer::questionId).toList(), QuestionType.MCQ);

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
      if (isCorrect) correctCount++;
      else wrongIds.add(question.getId());

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

      persistUserAnswer(userId, question, answer.label(), isCorrect, 100, session, item, "MICRO_MCQ");
      pushProgressHook(userId, ExamMode.WRITTEN, QuestionType.MCQ, isCorrect, 100, question.getId());
      updateProgress(userId, question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);
    }

    boolean allCorrect = !items.isEmpty() && wrongIds.isEmpty();
    int scorePct = items.isEmpty() ? 0 : (correctCount * 100) / items.size();
    boolean mcqCompleted = allCorrect;  // 모든 문제를 맞춰야 완료

    // 3. LearningStep (MCQ) 업데이트 (이전 메타데이터 불러와서 누적)
    Map<String, Object> prevMcqMeta = parseJson(mcqStep.getMetadataJson());
    Map<String, Object> mcqMeta = new HashMap<>(prevMcqMeta);
    
    // 누적 로직
    int prevTotal = readInt(prevMcqMeta, "total");
    int prevCorrect = readInt(prevMcqMeta, "correct");
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds = prevMcqMeta.get("wrongQuestionIds") instanceof List<?>
        ? (List<Long>) prevMcqMeta.get("wrongQuestionIds")
        : new ArrayList<>();
    
    int newTotal = prevTotal + req.answers().size();
    int newCorrect = prevCorrect + correctCount;
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    allWrongIds.addAll(wrongIds);
    boolean prevCompleted = Boolean.TRUE.equals(prevMcqMeta.get("completed"));
    boolean finalCompleted = prevCompleted || mcqCompleted;
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;
    
    mcqMeta.put("total", newTotal);
    mcqMeta.put("correct", newCorrect);
    mcqMeta.put("completed", finalCompleted);
    mcqMeta.put("scorePct", accumulatedScorePct);
    mcqMeta.put("wrongQuestionIds", allWrongIds);
    mcqMeta.put("lastSubmittedAt", Instant.now().toString());
    
    String metadataJson = toJson(mcqMeta);
    learningSessionService.updateStepStatus(mcqStep, "COMPLETE", accumulatedScorePct, metadataJson);

    // 4. 진정한 완료 설정 (MCQ 완료 시)
    if (finalCompleted && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    }

    // 5. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mcq", mcqMeta);

    // 6. MCQ 5문제 완료 시 세션 종료
    if (newTotal >= MCQ_SIZE) {
      sessionManager.closeSession(session, accumulatedScorePct, Map.of("finalScorePct", accumulatedScorePct));
    } else {
      sessionManager.updateStatus(session, "OPEN");
    }

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "MICRO",
        "MICRO_MCQ",
        mcqCompleted ? "COMPLETE" : "IN_PROGRESS",
        mcqCompleted ? "SUMMARY" : "MICRO_MCQ",
        sessionManager.loadMeta(session),
        new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds),
        learningSession.getId()
    );
  }

  /* ========================= 리뷰 ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<ReviewDtos.ReviewSet> reviewSet(Long rootTopicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    Set<Long> topicIds = topicTreeService.descendantsOf(rootTopicId);
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
        new ReviewDtos.ReviewSet(items),
        null  // REVIEW는 LearningSession을 사용하지 않음
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> reviewSubmitWritten(WrittenDtos.McqSubmitReq req,
                                                                              Long rootTopicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    Set<Long> rawIds = topicTreeService.descendantsOf(rootTopicId);
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
        userId, rootTopicId, ExamMode.WRITTEN, REVIEW_SIZE);
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
      if (isCorrect) correctCount++;
      else wrongIds.add(question.getId());

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

      persistUserAnswer(userId, question, answer.label(), isCorrect, 100, session, item, "REVIEW_MCQ");
      pushProgressHook(userId, ExamMode.WRITTEN, QuestionType.MCQ, isCorrect, 100, question.getId());
      updateProgress(userId, question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);
    }

    boolean allCorrect = !items.isEmpty() && wrongIds.isEmpty();

    Map<String, Object> prevReviewMeta = sessionManager.loadStepMeta(session, "review");
    boolean everCompleted = Boolean.TRUE.equals(prevReviewMeta.get("completed"));
    boolean finalCompleted = everCompleted || allCorrect;

    Map<String, Object> reviewMeta = new HashMap<>(prevReviewMeta);
    reviewMeta.put("total", req.answers().size());
    reviewMeta.put("correct", correctCount);
    reviewMeta.put("completed", finalCompleted);
    reviewMeta.put("wrongQuestionIds", wrongIds);
    reviewMeta.put("lastSubmittedAt", Instant.now().toString());
    sessionManager.saveStepMeta(session, "review", reviewMeta);

    if (!everCompleted && allCorrect) {
      double scorePct = req.answers().isEmpty() ? 0.0 : (correctCount * 100.0) / req.answers().size();
      sessionManager.closeSession(session, scorePct, true, Map.of("reviewScorePct", scorePct));
    } else if (!everCompleted) {
      sessionManager.updateStatus(session, "OPEN");
    }

    if (finalCompleted && allCorrect && !Boolean.TRUE.equals(session.getXpGranted())) {
      try {
        progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
            userId,
            ExamMode.WRITTEN.name(),
            "REVIEW",
            rootTopicId
        ));
        sessionManager.markXpGranted(session);
      } catch (Exception ignored) {
      }
    }

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "REVIEW",
        "REVIEW_SET",
        finalCompleted ? "COMPLETE" : "IN_PROGRESS",
        finalCompleted ? "REVIEW_SUMMARY" : "REVIEW_SET",
        sessionManager.loadMeta(session),
        new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds),
        null  // REVIEW는 LearningSession을 사용하지 않음
    );
  }

  /* ========================= 요약 ========================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(Long topicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회
    LearningSession learningSession = null;
    try {
      learningSession = learningSessionService.getOrCreateLearningSession(userId, topicId, ExamMode.WRITTEN);
    } catch (Exception e) {
      // 세션이 없으면 null로 처리
    }
    
    // 2. StudySession 조회 (하위 호환성을 위해)
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

    if (learningSession != null) {
      try {
        // LearningStep에서 메타데이터 추출
        LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
        LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
        
        if (miniStep.getMetadataJson() != null && !miniStep.getMetadataJson().isBlank()) {
          Map<String, Object> miniMeta = parseJson(miniStep.getMetadataJson());
          miniTotal = readInt(miniMeta, "total");
          miniCorrect = readInt(miniMeta, "correct");
          miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
        }
        
        if (mcqStep.getMetadataJson() != null && !mcqStep.getMetadataJson().isBlank()) {
          Map<String, Object> mcqMeta = parseJson(mcqStep.getMetadataJson());
          mcqTotal = readInt(mcqMeta, "total");
          mcqCorrect = readInt(mcqMeta, "correct");
          mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));
        }
      } catch (Exception e) {
        // LearningStep이 없거나 파싱 실패 시 기존 방식으로 fallback
        if (session != null) {
          Map<String, Object> miniMeta = sessionManager.loadStepMeta(session, "mini");
          Map<String, Object> mcqMeta = sessionManager.loadStepMeta(session, "mcq");
          
          miniTotal = readInt(miniMeta, "total");
          miniCorrect = readInt(miniMeta, "correct");
          miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
          
          mcqTotal = readInt(mcqMeta, "total");
          mcqCorrect = readInt(mcqMeta, "correct");
          mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));
        }
      }
      
      // StudySession이 있으면 sessionId 사용
      if (session != null) {
        sessionId = session.getId();
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
    } else if (session != null) {
      // LearningSession이 없지만 StudySession이 있는 경우 (기존 데이터)
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
    }

    int totalSolved = miniTotal + mcqTotal;
    int totalCorrect = miniCorrect + mcqCorrect;
    boolean completed = miniPassed && mcqCompleted;

    String topicTitle = "";
    try {
      CurriculumGateway.CurriculumConcept curriculum = curriculumGateway.getConceptWithTopic(topicId);
      topicTitle = curriculum.topicTitle();
    } catch (Exception ignored) {
    }

    String summaryText = aiExplanationService.summarizeWritten(
        topicTitle,
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
        summaryText,
        completed
    );

    String status;
    if (learningSession == null) {
      status = "NOT_STARTED";
    } else {
      status = completed ? "COMPLETE" : "IN_PROGRESS";
    }

    // 진정한 완료(MCQ 완료)일 때만 XP 지급
    boolean trulyCompleted = learningSession != null && Boolean.TRUE.equals(learningSession.getTrulyCompleted());
    
    if (trulyCompleted && sessionId != null && session != null) {
      if (!Boolean.TRUE.equals(session.getXpGranted())) {
        try {
          progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
              userId,
              ExamMode.WRITTEN.name(),
              "MICRO",
              topicId
          ));
          sessionManager.markXpGranted(session);
          if (!Boolean.TRUE.equals(session.getCompleted())) {
            double scorePct = totalSolved == 0 ? 0.0 : (totalCorrect * 100.0) / totalSolved;
            sessionManager.closeSession(session, scorePct, completed, Map.of());
          }
        } catch (Exception ignored) {
        }
      }
    }

    Long learningSessionId = learningSession != null ? learningSession.getId() : null;
    
    return new FlowDtos.StepEnvelope<>(
        sessionId,
        "MICRO",
        "MICRO_SUMMARY",
        status,
        null,
        meta,
        payload,
        learningSessionId
    );
  }

  /* ========================= Wrong Recap (세션 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapByLearningSession(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    // LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    
    // MCQ 단계의 LearningStep 조회
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // StudySession 조회
    StudySession session = mcqStep.getStudySession();
    if (session == null) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }
    
    // 기존 wrongRecapBySession 로직 재사용
    return wrongRecapBySession(session.getId(), "MICRO_MCQ");
  }

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapBySession(Long sessionId, String stepCode) {
    String userId = AuthUserUtil.getCurrentUserId();

    StudySession session = sessionManager.getSession(sessionId);
    if (!session.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }

    String source = mapStepToSource(stepCode);

    List<UserAnswer> wrongAnswers = userAnswerRepository.findByUserId(userId).stream()
        .filter(ans -> Objects.equals(ans.getSessionId(), sessionId))
        .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
        .filter(ans -> Objects.equals(source, ans.getSource()))
        .filter(ans -> Boolean.FALSE.equals(ans.getCorrect()))
        .sorted(Comparator.comparing(UserAnswer::getAnsweredAt))
        .toList();

    if (wrongAnswers.isEmpty()) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }

    LinkedHashSet<Long> qIds = wrongAnswers.stream()
        .map(UserAnswer::getQuestionId)
        .collect(Collectors.toCollection(LinkedHashSet::new));

    Map<Long, Question> questionCache = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == session.getExamMode())
        .collect(Collectors.toMap(Question::getId, q -> q));

    List<WrongRecapDtos.WrongRecapSet.Item> items = wrongAnswers.stream()
        .map(ans -> {
          Question q = questionCache.get(ans.getQuestionId());
          if (q == null) return null;
          return buildWrongRecapItem(q, ans);
        })
        .filter(Objects::nonNull)
        .toList();

    return new WrongRecapDtos.WrongRecapSet(items);
  }

  /* ========================= Wrong Recap (토픽/전체 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecap(Long topicId, int limit) {
    String userId = AuthUserUtil.getCurrentUserId();

    List<UserAnswer> wrongAnswers = userAnswerRepository.findByUserId(userId).stream()
        .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN) // 🔹 필기만
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

  /* ========================= Wrong Recap (문제 ID 목록 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapByIds(String ids) {
    String userId = AuthUserUtil.getCurrentUserId();

    // "1,2,3" 형태의 문자열 → Long 리스트로 파싱
    List<Long> idList = Arrays.stream(ids.split(","))
        .map(String::trim)
        .filter(s -> !s.isEmpty())
        .map(Long::parseLong)
        .toList();

    if (idList.isEmpty()) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }

    // 질문 캐시: 지정된 ID + 필기(WRITTEN)만
    LinkedHashSet<Long> questionIds = new LinkedHashSet<>(idList);

    Map<Long, Question> questionCache = questionRepository.findByIdIn(questionIds).stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 사용자 기준 최신 정답 맵 (이미 wrongRecap(...) 에서 쓰는 헬퍼 재사용)
    Map<Long, UserAnswer> latestAnswers = latestAnswerMap(userId);

    // 요청 순서를 유지하면서 WrongRecap 아이템 생성
    List<WrongRecapDtos.WrongRecapSet.Item> items = idList.stream()
        .map(questionCache::get)
        .filter(Objects::nonNull)
        .map(question -> toWrongRecapItem(question, latestAnswers))
        .toList();

    return new WrongRecapDtos.WrongRecapSet(items);
  }


  /* ========================= 즉시 채점 ========================= */

  public WrittenDtos.MiniGradeOneResp gradeOneMini(WrittenDtos.MiniGradeOneReq req) {
    FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> envelope =
        submitMini(new WrittenDtos.MiniSubmitReq(
            req.topicId(),
            List.of(new WrittenDtos.MiniAnswer(req.questionId(), req.answer()))
        ));

    WrittenDtos.MiniSubmitResp resp = envelope.payload();
    WrittenDtos.MiniSubmitItem item = resp.items().isEmpty()
        ? new WrittenDtos.MiniSubmitItem(req.questionId(), false, "", "")
        : resp.items().get(0);

    return new WrittenDtos.MiniGradeOneResp(
        item.correct(),
        item.explanation()
    );
  }

  public WrittenDtos.McqGradeOneResp gradeOneMcq(WrittenDtos.McqGradeOneReq req) {
    FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> envelope =
        submitMcq(new WrittenDtos.McqSubmitReq(
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

  // ====== MCQ 보기 로딩 / 정답 조회 ======

  private List<WrittenDtos.McqChoice> loadChoices(Long questionId) {
    return choiceRepository.findByQuestionIdOrderByLabelAsc(questionId).stream()
        .map(choice -> new WrittenDtos.McqChoice(
            choice.getLabel(),
            choice.getContent()
        ))
        .toList();
  }

  private List<ReviewDtos.ReviewQuestion.Choice> loadReviewChoices(Long questionId) {
    return choiceRepository.findByQuestionIdOrderByLabelAsc(questionId).stream()
        .map(choice -> new ReviewDtos.ReviewQuestion.Choice(
            choice.getLabel(),
            choice.getContent()
        ))
        .toList();
  }

  private String resolveCorrectChoice(Long questionId) {
    return choiceRepository.findFirstByQuestionIdAndCorrectTrue(questionId)
        .map(QuestionChoice::getLabel)
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

  // 공통: Question + UserAnswer로 WrongRecap Item 생성
  private WrongRecapDtos.WrongRecapSet.Item buildWrongRecapItem(Question question, UserAnswer answer) {
    String stem = Optional.ofNullable(question.getStem()).orElse("");
    String solution = Optional.ofNullable(question.getSolutionText()).orElse("");
    String correctAnswer = switch (question.getType()) {
      case OX -> Optional.ofNullable(question.getAnswerKey()).orElse("");
      case MCQ -> resolveCorrectChoice(question.getId());
      default -> "";
    };

    String userAnswerJson = (answer == null)
        ? "{}"
        : Optional.ofNullable(answer.getUserAnswerJson()).orElse("{}");

    return new WrongRecapDtos.WrongRecapSet.Item(
        question.getId(),
        question.getType().name(),
        stem,
        userAnswerJson,
        correctAnswer,
        solution,
        question.getImageUrl()
    );
  }

  // 토픽/전체 오답노트용: latestAnswerMap 기반
  private WrongRecapDtos.WrongRecapSet.Item toWrongRecapItem(Question question, Map<Long, UserAnswer> latestAnswers) {
    UserAnswer latest = latestAnswers.get(question.getId());
    return buildWrongRecapItem(question, latest);
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

  private Map<String, Object> parseJson(String json) {
    if (json == null || json.isBlank()) {
      return new HashMap<>();
    }
    try {
      return objectMapper.readValue(json, new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {});
    } catch (JsonProcessingException e) {
      return new HashMap<>();
    }
  }

  private Map<Long, UserAnswer> latestAnswerMap(String userId) {
    return userAnswerRepository.findByUserId(userId).stream()
        .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN) // 🔹 필기만
        .collect(Collectors.groupingBy(
            UserAnswer::getQuestionId,
            Collectors.collectingAndThen(
                Collectors.maxBy(Comparator.comparing(UserAnswer::getAnsweredAt)),
                opt -> opt.orElse(null)
            )
        ));
  }

  // stepCode(MICRO_OX / MICRO_MCQ / REVIEW ...) → UserAnswer.source 로 매핑
  private String mapStepToSource(String stepCode) {
    if (stepCode == null || stepCode.isBlank()) {
      return "MICRO_MCQ";
    }
    return switch (stepCode) {
      case "MICRO_OX", "MICRO_MINI" -> "MICRO_MINI";    // 필기 Micro OX
      case "MICRO_MCQ" -> "MICRO_MCQ";                  // 필기 Micro MCQ
      case "REVIEW", "REVIEW_SET", "REVIEW_MCQ" -> "REVIEW_MCQ"; // 필기 Review
      default -> stepCode; // 혹시 다른 모드(source)를 그대로 넘기고 싶을 때
    };
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
}
