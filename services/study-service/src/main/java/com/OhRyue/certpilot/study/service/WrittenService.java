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
import java.util.Comparator;
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
  private final LearningStepRepository learningStepRepository;
  private final AIExplanationService aiExplanationService;
  private final TopicTreeService topicTreeService;
  private final ProgressHookClient progressHookClient;
  private final ObjectMapper objectMapper;
  private final CurriculumGateway curriculumGateway;

  /* ========================= 개념 ========================= */

  @Transactional(readOnly = true)
  public WrittenDtos.ConceptResp loadConcept(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    // LearningSession 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }

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

  /**
   * CONCEPT 단계 완료 처리
   * 주의: 이제 advance API를 통해 단계 전이를 수행해야 합니다.
   * 이 메서드는 하위 호환성을 위해 유지되지만, 내부적으로는 아무 작업도 하지 않습니다.
   * 프론트엔드는 POST /api/study/session/advance를 호출해야 합니다.
   */
  @Transactional
  public void completeConcept(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    // 상태 변경은 advance API를 통해 수행되어야 함
    // 하위 호환성을 위해 메서드는 유지하지만 실제 작업은 하지 않음
  }

  /* ========================= 미니체크(OX) ========================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> miniSet(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }

    // 2. MINI 단계 조회
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    StudySession studySession = miniStep.getStudySession();
    
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다. 세션을 먼저 시작해주세요.");
    }

    // 3. 세션에 할당된 문제 조회 (랜덤이 아님!)
    List<StudySessionItem> items = sessionManager.items(studySession.getId());
    List<Long> questionIds = items.stream()
        .map(StudySessionItem::getQuestionId)
        .toList();

    if (questionIds.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // 4. 문제 상세 정보 조회
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환
    List<WrittenDtos.MiniQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          return new WrittenDtos.MiniQuestion(q.getId(), Optional.ofNullable(q.getStem()).orElse(""));
        })
        .toList();

    // 6. 단계 상태 확인
    String status = miniStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    
    // 상태 변경은 advance API를 통해 수행되어야 함
    // 단계가 READY 상태이면 IN_PROGRESS로 표시만 함 (실제 변경은 advance에서)
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "MICRO",
        "MICRO_MINI",
        completed ? "COMPLETE" : "IN_PROGRESS",
        completed ? "MCQ" : null,
        sessionManager.loadMeta(studySession),
        new WrittenDtos.MiniSet(questions),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(Long learningSessionId, WrittenDtos.MiniSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.topicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    
    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = miniStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }
    
    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());
    
    for (WrittenDtos.MiniAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }
    
    Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
        .map(WrittenDtos.MiniAnswer::questionId).toList(), QuestionType.OX);
    
    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));

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

      int orderNo = questionOrderMap.get(question.getId());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
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

    // 4. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mini", miniMeta);

    // 5. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MINI 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    miniStep.setMetadataJson(metadataJson);
    miniStep.setScorePct(accumulatedScorePct);
    miniStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(miniStep);

    // 상태는 메타데이터 기반으로 판단 (실제 상태 변경은 advance에서)
    String status = newTotal >= MINI_SIZE ? "COMPLETE" : "IN_PROGRESS";
    String nextStep = newTotal >= MINI_SIZE ? "MCQ" : null;
    
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

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<WrittenDtos.McqSet> mcqSet(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }

    // 2. MCQ 단계 조회
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    StudySession studySession = mcqStep.getStudySession();
    
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다. 세션을 먼저 시작해주세요.");
    }

    // 3. 세션에 할당된 문제 조회 (랜덤이 아님!)
    List<StudySessionItem> items = sessionManager.items(studySession.getId());
    List<Long> questionIds = items.stream()
        .map(StudySessionItem::getQuestionId)
        .toList();

    if (questionIds.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // 4. 문제 상세 정보 조회
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환
    List<WrittenDtos.McqQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          return new WrittenDtos.McqQuestion(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              loadChoices(q.getId()),
              q.getImageUrl()
          );
        })
        .toList();

    // 6. 단계 상태 확인
    String status = mcqStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    
    // 상태 변경은 advance API를 통해 수행되어야 함
    // 단계가 READY 상태이면 IN_PROGRESS로 표시만 함 (실제 변경은 advance에서)
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "MICRO",
        "MICRO_MCQ",
        completed ? "COMPLETE" : "IN_PROGRESS",
        completed ? "REVIEW_WRONG" : null,
        sessionManager.loadMeta(studySession),
        new WrittenDtos.McqSet(questions),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> submitMcq(Long learningSessionId, WrittenDtos.McqSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.topicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = mcqStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }
    
    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());
    
    for (WrittenDtos.McqAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }

    Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
        .map(WrittenDtos.McqAnswer::questionId).toList(), QuestionType.MCQ);

    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));

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

      int orderNo = questionOrderMap.get(question.getId());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
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

    // 4. 진정한 완료 설정 (MCQ 완료 시)
    if (finalCompleted && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    }

    // 5. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mcq", mcqMeta);

    // 6. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MCQ 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    mcqStep.setMetadataJson(metadataJson);
    mcqStep.setScorePct(accumulatedScorePct);
    mcqStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(mcqStep);

    // 상태는 메타데이터 기반으로 판단 (실제 상태 변경은 advance에서)
    String status = newTotal >= MCQ_SIZE ? "COMPLETE" : "IN_PROGRESS";
    String nextStep = newTotal >= MCQ_SIZE ? "REVIEW_WRONG" : null;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "MICRO",
        "MICRO_MCQ",
        status,
        nextStep,
        sessionManager.loadMeta(session),
        new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds),
        learningSession.getId()
    );
  }

  /* ========================= 리뷰 ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<ReviewDtos.ReviewSet> reviewSet(Long rootTopicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(rootTopicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    if (!"REVIEW".equals(learningSession.getMode())) {
      throw new IllegalStateException("Review 모드가 아닙니다.");
    }

    // 2. MCQ 단계 조회
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    StudySession studySession = mcqStep.getStudySession();
    
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다. 세션을 먼저 시작해주세요.");
    }

    // 3. 세션에 할당된 문제 조회 (랜덤이 아님!)
    List<StudySessionItem> items = sessionManager.items(studySession.getId());
    List<Long> questionIds = items.stream()
        .map(StudySessionItem::getQuestionId)
        .toList();

    if (questionIds.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // 4. 문제 상세 정보 조회
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환
    List<ReviewDtos.ReviewQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          return new ReviewDtos.ReviewQuestion(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              loadReviewChoices(q.getId()),
              q.getImageUrl()
          );
        })
        .toList();

    // 6. 단계 상태 확인
    String status = mcqStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    
    // 상태 변경은 advance API를 통해 수행되어야 함
    // 단계가 READY 상태이면 IN_PROGRESS로 표시만 함 (실제 변경은 advance에서)
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "REVIEW",
        "REVIEW_MCQ",
        completed ? "COMPLETE" : "IN_PROGRESS",
        completed ? "REVIEW_WRONG" : null,
        sessionManager.loadMeta(studySession),
        new ReviewDtos.ReviewSet(questions),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.McqSubmitResp> reviewSubmitWritten(Long learningSessionId, WrittenDtos.McqSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.topicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    if (!"REVIEW".equals(learningSession.getMode())) {
      throw new IllegalStateException("Review 모드가 아닙니다.");
    }
    
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = mcqStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }
    
    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());
    
    for (WrittenDtos.McqAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }

    Map<Long, Question> questionMap = fetchQuestions(req.answers().stream()
        .map(WrittenDtos.McqAnswer::questionId).toList(), QuestionType.MCQ);

    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));

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

      int orderNo = questionOrderMap.get(question.getId());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
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

    // 4. 진정한 완료 설정 (MCQ 완료 시)
    if (finalCompleted && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    }

    // 5. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mcq", mcqMeta);

    // 6. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MCQ 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    mcqStep.setMetadataJson(metadataJson);
    mcqStep.setScorePct(accumulatedScorePct);
    mcqStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(mcqStep);

    // 상태는 메타데이터 기반으로 판단 (실제 상태 변경은 advance에서)
    String status = newTotal >= REVIEW_SIZE ? "COMPLETE" : "IN_PROGRESS";
    String nextStep = newTotal >= REVIEW_SIZE ? "REVIEW_WRONG" : null;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "REVIEW",
        "REVIEW_MCQ",
        status,
        nextStep,
        sessionManager.loadMeta(session),
        new WrittenDtos.McqSubmitResp(req.answers().size(), correctCount, items, wrongIds),
        learningSession.getId()
    );
  }

  /* ========================= 요약 ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> reviewSummary(Long rootTopicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(rootTopicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    if (!"REVIEW".equals(learningSession.getMode())) {
      throw new IllegalStateException("Review 모드가 아닙니다.");
    }
    
    // 2. StudySession 조회 (MCQ 세션 사용)
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    StudySession session = mcqStep.getStudySession();

    // 3. LearningStep에서 메타데이터 추출
    Map<String, Object> mcqMeta = parseJson(mcqStep.getMetadataJson());
    
    int mcqTotal = readInt(mcqMeta, "total");
    int mcqCorrect = readInt(mcqMeta, "correct");
    boolean mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));
    
    // 4. 약점 태그 계산
    List<String> weakTags = List.of();
    Map<String, Object> meta = Map.of();
    Long sessionId = null;
    
    if (session != null) {
      sessionId = session.getId();
      meta = sessionManager.loadMeta(session);

      List<UserAnswer> sessionAnswers = userAnswerRepository.findByUserIdAndSessionId(userId, sessionId).stream()
          .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
          .toList();
      Set<Long> questionIds = sessionAnswers.stream().map(UserAnswer::getQuestionId).collect(Collectors.toSet());
      Map<Long, Question> questionCache = questionRepository.findByIdIn(questionIds).stream()
          .collect(Collectors.toMap(Question::getId, q -> q));
      List<UserAnswer> answers = sessionAnswers.stream()
          .filter(ans -> questionCache.containsKey(ans.getQuestionId()))
          .toList();
      weakTags = computeWeakTags(answers, questionCache);
    }

    boolean completed = mcqCompleted;

    String topicTitle = "";
    try {
      CurriculumGateway.CurriculumConcept curriculum = curriculumGateway.getConceptWithTopic(rootTopicId);
      topicTitle = curriculum.topicTitle();
    } catch (Exception ignored) {
    }

    String summaryText = aiExplanationService.summarizeWritten(
        topicTitle,
        mcqTotal,
        mcqCorrect,
        weakTags
    );

    WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
        0,  // Review 모드에는 MINI 없음
        0,
        false,
        mcqTotal,
        mcqCorrect,
        summaryText,
        completed
    );

    String status = completed ? "COMPLETE" : "IN_PROGRESS";

    // 진정한 완료(MCQ 완료)일 때만 XP 지급
    boolean trulyCompleted = learningSession != null && Boolean.TRUE.equals(learningSession.getTrulyCompleted());
    
    if (trulyCompleted && sessionId != null && session != null) {
      if (!Boolean.TRUE.equals(session.getXpGranted())) {
        try {
          progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
              userId,
              ExamMode.WRITTEN.name(),
              "REVIEW",
              rootTopicId
          ));
          sessionManager.markXpGranted(session);
          if (!Boolean.TRUE.equals(session.getCompleted())) {
            double scorePct = mcqTotal == 0 ? 0.0 : (mcqCorrect * 100.0) / mcqTotal;
            sessionManager.closeSession(session, scorePct, completed, Map.of());
          }
        } catch (Exception ignored) {
        }
      }
    }

    // SUMMARY 단계는 advance API를 통해 완료 처리되어야 함
    // 상태 변경은 advance에서 수행되므로 여기서는 하지 않음
    
    return new FlowDtos.StepEnvelope<>(
        sessionId,
        "REVIEW",
        "REVIEW_SUMMARY",
        "COMPLETE",
        null,
        meta,
        payload,
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    
    // 2. StudySession 조회 (MCQ 세션 사용)
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    StudySession session = mcqStep.getStudySession();

    // 3. LearningStep에서 메타데이터 추출
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    
    Map<String, Object> miniMeta = parseJson(miniStep.getMetadataJson());
    Map<String, Object> mcqMeta = parseJson(mcqStep.getMetadataJson());
    
    int miniTotal = readInt(miniMeta, "total");
    int miniCorrect = readInt(miniMeta, "correct");
    boolean miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
    
    int mcqTotal = readInt(mcqMeta, "total");
    int mcqCorrect = readInt(mcqMeta, "correct");
    boolean mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));
    
    // 4. 약점 태그 계산
    List<String> weakTags = List.of();
    Map<String, Object> meta = Map.of();
    Long sessionId = null;
    
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

    // SUMMARY 단계는 advance API를 통해 완료 처리되어야 함
    // 상태 변경은 advance에서 수행되므로 여기서는 하지 않음
    
    return new FlowDtos.StepEnvelope<>(
        sessionId,
        "MICRO",
        "MICRO_SUMMARY",
        "COMPLETE",
        null,
        meta,
        payload,
        learningSession.getId()
    );
  }

  /* ========================= Wrong Recap (세션 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapByLearningSession(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    // LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    
    // 모드에 따라 적절한 단계 선택
    String stepCode;
    if ("REVIEW".equals(learningSession.getMode())) {
      stepCode = "REVIEW_MCQ";
    } else {
      stepCode = "MICRO_MCQ";
    }
    
    // MCQ 단계의 LearningStep 조회
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // StudySession 조회
    StudySession session = mcqStep.getStudySession();
    if (session == null) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }
    
    // 기존 wrongRecapBySession 로직 재사용
    return wrongRecapBySession(session.getId(), stepCode);
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

  @Transactional
  public WrittenDtos.MiniGradeOneResp gradeOneMini(Long learningSessionId, WrittenDtos.MiniGradeOneReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    
    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = miniStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }
    
    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());
    
    if (!allocatedQuestionIds.contains(req.questionId())) {
      throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + req.questionId());
    }
    
    // 4. 문제 조회 및 채점
    Question question = questionRepository.findById(req.questionId())
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.OX)
        .orElseThrow(() -> new NoSuchElementException("Question not found: " + req.questionId()));

    String correctAnswer = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
    String userAnswer = Boolean.TRUE.equals(req.answer()) ? "O" : "X";
    boolean isCorrect = correctAnswer.equalsIgnoreCase(userAnswer);
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

    // 5. 세션에 아이템 저장 (순서는 세션에 할당된 순서 사용)
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));
    int orderNo = questionOrderMap.get(question.getId());
    String answerJson = toJson(Map.of(
        "answer", userAnswer,
        "correct", isCorrect,
        "submittedAt", Instant.now().toString()
    ));

    StudySessionItem item = sessionManager.upsertItem(
        session,
        question.getId(),
        orderNo,
        answerJson,
        isCorrect,
        isCorrect ? 100 : 0,
        null
    );

    persistUserAnswer(userId, question, userAnswer, isCorrect, 100, session, item, "MICRO_MINI");
    pushProgressHook(userId, ExamMode.WRITTEN, QuestionType.OX, isCorrect, 100, question.getId());
    updateProgress(userId, question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);

    // 5. LearningStep 메타데이터 업데이트 (누적)
    Map<String, Object> prevMiniMeta = parseJson(miniStep.getMetadataJson());
    Map<String, Object> miniMeta = new HashMap<>(prevMiniMeta);
    
    int prevTotal = readInt(prevMiniMeta, "total");
    int prevCorrect = readInt(prevMiniMeta, "correct");
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds = prevMiniMeta.get("wrongQuestionIds") instanceof List<?> 
        ? (List<Long>) prevMiniMeta.get("wrongQuestionIds")
        : new ArrayList<>();
    
    int newTotal = prevTotal + 1;
    int newCorrect = prevCorrect + (isCorrect ? 1 : 0);
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    if (!isCorrect) {
      allWrongIds.add(question.getId());
    }
    boolean passedNow = newCorrect == newTotal;
    boolean everPassed = Boolean.TRUE.equals(prevMiniMeta.get("passed")) || passedNow;
    
    miniMeta.put("total", newTotal);
    miniMeta.put("correct", newCorrect);
    miniMeta.put("passed", everPassed);
    miniMeta.put("wrongQuestionIds", allWrongIds);
    miniMeta.put("lastSubmittedAt", Instant.now().toString());
    
    // 누적된 값으로 scorePct 재계산
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;
    
    String metadataJson = toJson(miniMeta);

    // 6. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mini", miniMeta);

    // 7. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MINI 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    miniStep.setMetadataJson(metadataJson);
    miniStep.setScorePct(accumulatedScorePct);
    miniStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(miniStep);

    return new WrittenDtos.MiniGradeOneResp(
        isCorrect,
        explanation,
        learningSession.getId()
    );
  }

  @Transactional
  public WrittenDtos.McqGradeOneResp gradeOneMcq(Long learningSessionId, WrittenDtos.McqGradeOneReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    LearningStep mcqStep = learningSessionService.getStep(learningSession, "MCQ");
    
    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = mcqStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }
    
    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());
    
    if (!allocatedQuestionIds.contains(req.questionId())) {
      throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + req.questionId());
    }
    
    // 4. 문제 조회 및 채점
    Question question = questionRepository.findById(req.questionId())
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .orElseThrow(() -> new NoSuchElementException("Question not found: " + req.questionId()));

    String correctLabel = resolveCorrectChoice(question.getId());
    boolean isCorrect = Objects.equals(correctLabel, req.label());
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

    // 5. 세션에 아이템 저장 (순서는 세션에 할당된 순서 사용)
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));
    int orderNo = questionOrderMap.get(question.getId());
    Map<String, Object> answerPayload = new HashMap<>();
    answerPayload.put("answer", req.label());
    answerPayload.put("correctLabel", correctLabel);
    answerPayload.put("correct", isCorrect);
    answerPayload.put("submittedAt", Instant.now().toString());

    StudySessionItem item = sessionManager.upsertItem(
        session,
        question.getId(),
        orderNo,
        toJson(answerPayload),
        isCorrect,
        isCorrect ? 100 : 0,
        null  // AI 해설 없음
    );

    // LearningSession 모드에 따라 source 결정
    String source = "REVIEW".equals(learningSession.getMode()) ? "REVIEW_MCQ" : "MICRO_MCQ";
    
    persistUserAnswer(userId, question, req.label(), isCorrect, 100, session, item, source);
    pushProgressHook(userId, ExamMode.WRITTEN, QuestionType.MCQ, isCorrect, 100, question.getId());
    updateProgress(userId, question.getTopicId(), ExamMode.WRITTEN, isCorrect, 100);

    // 5. LearningStep 메타데이터 업데이트 (누적)
    Map<String, Object> prevMcqMeta = parseJson(mcqStep.getMetadataJson());
    Map<String, Object> mcqMeta = new HashMap<>(prevMcqMeta);
    
    int prevTotal = readInt(prevMcqMeta, "total");
    int prevCorrect = readInt(prevMcqMeta, "correct");
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds = prevMcqMeta.get("wrongQuestionIds") instanceof List<?>
        ? (List<Long>) prevMcqMeta.get("wrongQuestionIds")
        : new ArrayList<>();
    
    int newTotal = prevTotal + 1;
    int newCorrect = prevCorrect + (isCorrect ? 1 : 0);
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    if (!isCorrect) {
      allWrongIds.add(question.getId());
    }
    boolean allCorrect = newCorrect == newTotal;
    boolean prevCompleted = Boolean.TRUE.equals(prevMcqMeta.get("completed"));
    boolean finalCompleted = prevCompleted || allCorrect;
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;
    
    mcqMeta.put("total", newTotal);
    mcqMeta.put("correct", newCorrect);
    mcqMeta.put("completed", finalCompleted);
    mcqMeta.put("scorePct", accumulatedScorePct);
    mcqMeta.put("wrongQuestionIds", allWrongIds);
    mcqMeta.put("lastSubmittedAt", Instant.now().toString());
    
    String metadataJson = toJson(mcqMeta);

    // 6. 진정한 완료 설정 (MCQ 완료 시)
    if (finalCompleted && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    }

    // 7. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "mcq", mcqMeta);

    // 8. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MCQ 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    mcqStep.setMetadataJson(metadataJson);
    mcqStep.setScorePct(accumulatedScorePct);
    mcqStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(mcqStep);

    // MCQ는 객관식이므로 AI 해설 없이 반환
    return new WrittenDtos.McqGradeOneResp(
        isCorrect,
        correctLabel,
        explanation,
        ""  // AI 해설 제거 (AI 호출하지 않음)
    );
  }

  /* ========================= 문제 상세 조회 ========================= */

  @Transactional(readOnly = true)
  public WrittenDtos.QuestionDetailResp getQuestionDetail(Long questionId) {
    Question question = questionRepository.findById(questionId)
        .orElseThrow(() -> new NoSuchElementException("Question not found: " + questionId));

    // 필기 문제만 조회 가능
    if (question.getMode() != ExamMode.WRITTEN) {
      throw new IllegalArgumentException("Written exam mode only. Question ID: " + questionId);
    }

    String stem = Optional.ofNullable(question.getStem()).orElse("");
    String type = question.getType().name();
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");
    String correctAnswer;
    List<WrittenDtos.McqChoice> choices;

    if (question.getType() == QuestionType.OX) {
      // OX 문제: answerKey에서 정답 가져오기
      correctAnswer = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
      choices = List.of(); // OX는 선택지 없음
    } else if (question.getType() == QuestionType.MCQ) {
      // MCQ 문제: 선택지와 정답 라벨 가져오기
      choices = loadChoices(questionId);
      correctAnswer = resolveCorrectChoice(questionId);
    } else {
      throw new IllegalArgumentException("Only OX and MCQ question types are supported. Question ID: " + questionId);
    }

    return new WrittenDtos.QuestionDetailResp(
        question.getId(),
        type,
        stem,
        choices,
        correctAnswer,
        explanation
    );
  }

  @Transactional(readOnly = true)
  public WrittenDtos.QuestionDetailListResp getQuestionDetails(List<Long> questionIds) {
    if (questionIds == null || questionIds.isEmpty()) {
      return new WrittenDtos.QuestionDetailListResp(List.of());
    }

    // 중복 제거 및 유효성 검사
    List<Long> uniqueIds = questionIds.stream()
        .filter(Objects::nonNull)
        .distinct()
        .toList();

    if (uniqueIds.isEmpty()) {
      return new WrittenDtos.QuestionDetailListResp(List.of());
    }

    // 문제 일괄 조회
    List<Question> questions = questionRepository.findByIdIn(uniqueIds);
    
    // 필기 문제만 필터링
    List<Question> writtenQuestions = questions.stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN)
        .filter(q -> q.getType() == QuestionType.OX || q.getType() == QuestionType.MCQ)
        .toList();

    if (writtenQuestions.isEmpty()) {
      return new WrittenDtos.QuestionDetailListResp(List.of());
    }

    // MCQ 문제 ID 목록 추출
    List<Long> mcqQuestionIds = writtenQuestions.stream()
        .filter(q -> q.getType() == QuestionType.MCQ)
        .map(Question::getId)
        .toList();

    // 선택지 일괄 조회 (MCQ만)
    final Map<Long, List<WrittenDtos.McqChoice>> choicesMap;
    if (!mcqQuestionIds.isEmpty()) {
      List<QuestionChoice> allChoices = choiceRepository.findByQuestionIdIn(mcqQuestionIds);
      Map<Long, List<WrittenDtos.McqChoice>> tempMap = allChoices.stream()
          .collect(Collectors.groupingBy(
              QuestionChoice::getQuestionId,
              Collectors.mapping(
                  choice -> new WrittenDtos.McqChoice(choice.getLabel(), choice.getContent()),
                  Collectors.toList()
              )
          ));
      
      // 각 문제별로 label 순서대로 정렬
      choicesMap = tempMap.entrySet().stream()
          .collect(Collectors.toMap(
              Map.Entry::getKey,
              entry -> entry.getValue().stream()
                  .sorted(Comparator.comparing(WrittenDtos.McqChoice::label))
                  .toList()
          ));
    } else {
      choicesMap = new HashMap<>();
    }

    // 정답 일괄 조회 (MCQ만)
    Map<Long, String> correctAnswerMap = new HashMap<>();
    if (!mcqQuestionIds.isEmpty()) {
      for (Long qId : mcqQuestionIds) {
        String correctLabel = choiceRepository.findFirstByQuestionIdAndCorrectTrue(qId)
            .map(QuestionChoice::getLabel)
            .orElse("");
        correctAnswerMap.put(qId, correctLabel);
      }
    }

    // 요청 순서 유지하면서 응답 생성
    Map<Long, Question> questionMap = writtenQuestions.stream()
        .collect(Collectors.toMap(Question::getId, q -> q));

    List<WrittenDtos.QuestionDetailResp> results = uniqueIds.stream()
        .map(questionMap::get)
        .filter(Objects::nonNull)
        .map(question -> {
          String stem = Optional.ofNullable(question.getStem()).orElse("");
          String type = question.getType().name();
          String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");
          String correctAnswer;
          List<WrittenDtos.McqChoice> choices;

          if (question.getType() == QuestionType.OX) {
            correctAnswer = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
            choices = List.of();
          } else {
            choices = choicesMap.getOrDefault(question.getId(), List.of());
            correctAnswer = correctAnswerMap.getOrDefault(question.getId(), "");
          }

          return new WrittenDtos.QuestionDetailResp(
              question.getId(),
              type,
              stem,
              choices,
              correctAnswer,
              explanation
          );
        })
        .toList();

    return new WrittenDtos.QuestionDetailListResp(results);
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
        question.getImageUrl(),
        null,  // 필기는 AI 해설 미사용
        null   // 필기는 AI 해설 실패 여부 미사용
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
