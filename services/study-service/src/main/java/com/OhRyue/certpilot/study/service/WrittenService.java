package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.client.ProgressXpClient;
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
  private static final int REVIEW_SIZE = 20; // REVIEW는 20문제

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
  private final ProgressXpClient progressXpClient;
  private final ObjectMapper objectMapper;
  private final CurriculumGateway curriculumGateway;
  private final TagQueryService tagQueryService;

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

    // 5. 태그 정보 조회
    Map<Long, List<com.OhRyue.common.dto.TagViewDto>> tagsByQuestionId = getTagsByQuestionIds(questionIds);

    // 6. 순서대로 문제 반환
    List<WrittenDtos.MiniQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          List<com.OhRyue.common.dto.TagViewDto> tags = tagsByQuestionId.getOrDefault(q.getId(), List.of());
          return new WrittenDtos.MiniQuestion(q.getId(), Optional.ofNullable(q.getStem()).orElse(""), tags);
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

    // 5. 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    // study_session_item 기준으로 실제 제출된 문제 수 확인
    List<StudySessionItem> allItems = sessionManager.items(session.getId());
    long answeredCount = allItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allQuestionsAnswered = answeredCount >= MINI_SIZE && answeredCount == allItems.size();
    
    // 6. 세션이 완료되었을 때 finalizeStudySession 호출
    if (allQuestionsAnswered && session != null && !Boolean.TRUE.equals(session.getCompleted())) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      
      // LearningStep 메타데이터에 wrongQuestionIds 추가 (하위 호환성)
      miniMeta.put("total", result.total());
      miniMeta.put("correct", result.correct());
      miniMeta.put("scorePct", (int) Math.round(result.scorePct()));
      miniMeta.put("passed", result.passed());
      miniMeta.put("completed", true);
      miniMeta.put("wrongQuestionIds", wrongQuestionIds);
      miniStep.setMetadataJson(toJson(miniMeta));
      miniStep.setScorePct((int) Math.round(result.scorePct()));
      miniStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(miniStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      miniStep.setMetadataJson(metadataJson);
      miniStep.setScorePct(accumulatedScorePct);
      miniStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(miniStep);
    }

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

    // 5. 태그 정보 조회
    Map<Long, List<com.OhRyue.common.dto.TagViewDto>> tagsByQuestionId = getTagsByQuestionIds(questionIds);

    // 6. 순서대로 문제 반환
    List<WrittenDtos.McqQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          List<com.OhRyue.common.dto.TagViewDto> tags = tagsByQuestionId.getOrDefault(q.getId(), List.of());
          return new WrittenDtos.McqQuestion(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              loadChoices(q.getId()),
              q.getImageUrl(),
              tags
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

    // 4. 진정한 완료 설정 (MCQ 완료 시 - 모든 문제를 맞춰야 완료)
    boolean newlyCompleted = false;
    if (finalCompleted && allWrongIds.isEmpty() && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
      newlyCompleted = true;
    }

    // 5. 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    List<StudySessionItem> allMcqItems = sessionManager.items(session.getId());
    long mcqAnsweredCount = allMcqItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allMcqQuestionsAnswered = mcqAnsweredCount >= MCQ_SIZE && mcqAnsweredCount == allMcqItems.size();
    
    // summary_json에서도 완료 여부 확인
    Map<String, Object> summaryMcqMeta = sessionManager.loadStepMeta(session, "mcq");
    boolean mcqCompletedInSummary = Boolean.TRUE.equals(summaryMcqMeta.get("completed"));
    int mcqTotalInSummary = readInt(summaryMcqMeta, "total");
    
    // 5-2. 세션이 완료되었을 때 finalizeStudySession 호출
    // 핵심: 모든 문제가 제출되었거나, summary_json에 완료 정보가 있고 score_pct가 0이면 무조건 호출
    // MCQ 세션은 독립적으로 score_pct와 passed를 계산 (MINI와 무관)
    boolean shouldFinalize = false;
    if (session != null) {
      // 조건 1: 모든 문제가 제출되었으면 항상 호출
      if (allMcqQuestionsAnswered) {
        shouldFinalize = true;
      }
      // 조건 2: summary_json에 완료 정보가 있고, score_pct가 0이거나 NULL이면 무조건 호출
      // (summary_json에는 올바른 정보가 있는데 DB에는 0으로 저장되어 있으면 재계산 필요)
      if (mcqCompletedInSummary && mcqTotalInSummary >= MCQ_SIZE) {
        if (session.getScorePct() == null || session.getScorePct() == 0.0 || !Boolean.TRUE.equals(session.getPassed())) {
          shouldFinalize = true;
        }
      }
    }
    
    if (shouldFinalize) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      // MCQ 세션 자체의 passed는 MCQ만 고려 (MINI 무관)
      System.out.println("[MCQ Finalize] Calling finalizeStudySession for sessionId=" + session.getId() + 
                         ", scorePct before=" + session.getScorePct() + ", passed=" + session.getPassed());
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      System.out.println("[MCQ Finalize] After finalizeStudySession: total=" + result.total() + 
                         ", correct=" + result.correct() + ", scorePct=" + result.scorePct() + ", passed=" + result.passed());
      
      // 세션을 다시 조회하여 최신 상태 확인
      session = sessionManager.getSession(session.getId());
      System.out.println("[MCQ Finalize] Session reloaded: scorePct=" + session.getScorePct() + ", passed=" + session.getPassed());
      
      // LearningStep 메타데이터에 wrongQuestionIds 추가 (하위 호환성)
      // finalizeStudySession이 이미 LearningStep도 업데이트했으므로, 여기서는 wrongQuestionIds만 추가
      mcqStep = learningStepRepository.findById(mcqStep.getId()).orElse(mcqStep);
      Map<String, Object> finalMeta = parseJson(mcqStep.getMetadataJson());
      if (finalMeta == null || finalMeta.isEmpty()) {
        finalMeta = new HashMap<>();
      }
      finalMeta.put("wrongQuestionIds", allWrongIds);
      mcqStep.setMetadataJson(toJson(finalMeta));
      learningStepRepository.save(mcqStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      sessionManager.saveStepMeta(session, "mcq", mcqMeta);
      mcqStep.setMetadataJson(metadataJson);
      mcqStep.setScorePct(accumulatedScorePct);
      mcqStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(mcqStep);
    }

    // 6. [B] MICRO 완료 체크 및 XP 지급
    // summary_json에 완료 정보가 있거나 모든 문제가 제출되었으면 체크
    boolean mcqIsCompleted = allMcqQuestionsAnswered || (mcqCompletedInSummary && mcqTotalInSummary >= MCQ_SIZE);
    if (mcqIsCompleted && "MICRO".equals(learningSession.getMode())) {
      checkMicroCompletionAndXp(learningSession, session, ExamMode.WRITTEN);
    }

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

    // 5. 태그 정보 조회
    Map<Long, List<com.OhRyue.common.dto.TagViewDto>> tagsByQuestionId = getTagsByQuestionIds(questionIds);

    // 6. 순서대로 문제 반환
    List<ReviewDtos.ReviewQuestion> questions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          List<com.OhRyue.common.dto.TagViewDto> tags = tagsByQuestionId.getOrDefault(q.getId(), List.of());
          return new ReviewDtos.ReviewQuestion(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              loadReviewChoices(q.getId()),
              q.getImageUrl(),
              tags
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

    // 4. 진정한 완료 설정 (MCQ 완료 시 - 모든 문제를 맞춰야 완료)
    // REVIEW 모드가 아닌 경우에만 여기서 설정 (REVIEW는 finalizeStudySession 후 checkReviewCompletionAndXp에서 처리)
    boolean newlyCompleted = false;
    if (!"REVIEW".equals(learningSession.getMode()) && finalCompleted && allWrongIds.isEmpty() && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
      newlyCompleted = true;
    }

    // 5-1. 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    List<StudySessionItem> allReviewItems = sessionManager.items(session.getId());
    long reviewAnsweredCount = allReviewItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allReviewQuestionsAnswered = reviewAnsweredCount >= REVIEW_SIZE && reviewAnsweredCount == allReviewItems.size();
    
    // 5-2. 세션이 완료되었을 때 finalizeStudySession 호출
    // 모든 문제가 제출되었으면 finalizeStudySession 호출 (이미 완료된 경우도 재계산)
    if (allReviewQuestionsAnswered && session != null) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      
      // LearningStep 메타데이터 업데이트
      mcqMeta.put("total", result.total());
      mcqMeta.put("correct", result.correct());
      mcqMeta.put("scorePct", (int) Math.round(result.scorePct()));
      mcqMeta.put("passed", result.passed());
      mcqMeta.put("completed", true);
      mcqMeta.put("wrongQuestionIds", allWrongIds);
      
      // summary_json에도 저장
      sessionManager.saveStepMeta(session, "mcq", mcqMeta);
      
      // 세션 종료 처리: score_pct와 passed를 업데이트하고 CLOSED로 상태 변경
      Map<String, Object> currentMeta = sessionManager.loadMeta(session);
      sessionManager.closeSession(session, result.scorePct(), result.passed(), currentMeta);
      
      mcqStep.setMetadataJson(toJson(mcqMeta));
      mcqStep.setScorePct((int) Math.round(result.scorePct()));
      mcqStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(mcqStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      sessionManager.saveStepMeta(session, "mcq", mcqMeta);
      mcqStep.setMetadataJson(metadataJson);
      mcqStep.setScorePct(accumulatedScorePct);
      mcqStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(mcqStep);
    }

    // 6. [C] REVIEW 완료 체크 및 XP 지급
    if (allReviewQuestionsAnswered && "REVIEW".equals(learningSession.getMode())) {
      checkReviewCompletionAndXp(learningSession, session, ExamMode.WRITTEN);
    }

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

    // 3. 약점 태그 계산 및 summary_json 로드
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

    // 4. LearningStep과 summary_json에서 메타데이터 추출 (summary_json 우선)
    Map<String, Object> mcqMeta = parseJson(mcqStep.getMetadataJson());
    
    // summary_json에서 mcq 정보 가져오기 (우선순위 높음)
    if (session != null && !meta.isEmpty()) {
      Object mcqRaw = meta.get("mcq");
      if (mcqRaw instanceof Map<?, ?> mcqFromSummary) {
        Map<String, Object> mcqFromSummaryMap = new HashMap<>();
        mcqFromSummary.forEach((k, v) -> mcqFromSummaryMap.put(String.valueOf(k), v));
        // summary_json의 정보로 덮어쓰기
        mcqMeta.putAll(mcqFromSummaryMap);
      }
    }
    
    int mcqTotal = readInt(mcqMeta, "total");
    int mcqCorrect = readInt(mcqMeta, "correct");
    boolean mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));

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

    // XP 정보 초기값
    Integer earnedXp = null;
    Long totalXp = null;
    Integer level = null;
    Integer xpToNextLevel = null;
    Boolean leveledUp = null;
    Integer levelUpRewardPoints = null;

    String status = completed ? "COMPLETE" : "IN_PROGRESS";

    // XP 지급 (xp_granted=0이면 항상 지급, 정답률 기반)
    // 조건: xp_granted=0 (passed와 상관없이 정답률에 따라 XP 지급)
    if (sessionId != null && session != null && !Boolean.TRUE.equals(session.getXpGranted())) {
      try {
        // 정답률 계산 (summary_json에서 가져온 mcqMeta 우선 사용)
        double scorePct;
        
        // 1순위: mcqMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 가장 정확)
        Object scorePctObj = mcqMeta.get("scorePct");
        if (scorePctObj != null && scorePctObj instanceof Number) {
          scorePct = ((Number) scorePctObj).doubleValue();
          System.out.println("[WrittenService.reviewSummary] scorePct from mcqMeta: " + scorePct);
        } else if (mcqTotal > 0) {
          // 2순위: 계산하기
          scorePct = (mcqCorrect * 100.0) / mcqTotal;
          System.out.println("[WrittenService.reviewSummary] scorePct calculated: " + scorePct + 
                            " (mcqTotal=" + mcqTotal + ", mcqCorrect=" + mcqCorrect + ")");
        } else if (session.getScorePct() != null) {
          // 3순위: session에서 가져오기
          scorePct = session.getScorePct();
          System.out.println("[WrittenService.reviewSummary] scorePct from session: " + scorePct);
        } else {
          // 4순위: 기본값 0.0
          scorePct = 0.0;
          System.out.println("[WrittenService.reviewSummary] scorePct default: 0.0");
        }
        
        // scorePct는 반드시 0.0 이상 100.0 이하의 유효한 값이어야 함
        if (scorePct < 0.0 || scorePct > 100.0 || Double.isNaN(scorePct)) {
          System.err.println("[WrittenService.reviewSummary] Invalid scorePct: " + scorePct + ", using 0.0");
          scorePct = 0.0;
        }
        
        // 로깅: 실제 전달되는 scorePct 값 확인
        System.out.println("[WrittenService.reviewSummary] XP 지급 요청: sessionId=" + session.getId() + 
                          ", mcqTotal=" + mcqTotal + ", mcqCorrect=" + mcqCorrect + 
                          ", scorePct=" + scorePct);
        
        // 1. XP 지급 요청 (정답률 기반) - scorePct는 절대 null이 아님
        ProgressXpClient.XpEarnRequest xpRequest = new ProgressXpClient.XpEarnRequest(
            "WRITTEN_REVIEW",
            session.getId(),
            rootTopicId,
            scorePct  // 항상 유효한 Double 값 (null 아님)
        );
        
        System.out.println("[WrittenService.reviewSummary] XP 요청 상세: activityType=" + xpRequest.activityType() + 
                          ", sessionId=" + xpRequest.sessionId() + ", topicId=" + xpRequest.topicId() + 
                          ", scorePct=" + xpRequest.scorePct());
        
        ProgressXpClient.XpEarnResponse xpResp = progressXpClient.earnXp(xpRequest);
        
        // 2. XP 정보를 응답에 포함
        earnedXp = xpResp.earnedXp();
        totalXp = xpResp.totalXp();
        level = xpResp.level();
        xpToNextLevel = xpResp.xpToNextLevel();
        leveledUp = xpResp.leveledUp();
        levelUpRewardPoints = xpResp.levelUpRewardPoints();
        
        // 3. xp_granted 플래그 업데이트
        sessionManager.markXpGranted(session);
        
        // 4. 기존 hook도 호출 (다른 통계 처리용)
        try {
          progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
              userId,
              ExamMode.WRITTEN.name(),
              "REVIEW",
              rootTopicId,
              scorePct  // 정답률 전달
          ));
        } catch (Exception hookEx) {
          // hook 실패는 학습 흐름을 막지 않음
          System.err.println("Failed to call flow-complete hook: " + hookEx.getMessage());
        }
        
      } catch (Exception e) {
        // XP 지급 실패는 학습 흐름을 막지 않음, 로깅만 수행
        System.err.println("Failed to grant XP in reviewSummary (REVIEW): " + e.getMessage());
        e.printStackTrace();
      }
    }
    
    // REVIEW 세션의 score_pct와 passed 업데이트
    // summary_json에 완료 정보가 있으면 그것을 사용해서 직접 업데이트
    // 세션이 완료되었거나 (finished_at이 있거나, 모든 문제를 풀었거나) score_pct가 0이면 업데이트
    if (session != null && mcqTotal > 0) {
      // 세션이 완료되었는지 확인
      boolean sessionFinished = session.getFinishedAt() != null;
      boolean sessionCompleted = Boolean.TRUE.equals(session.getCompleted());
      
      // 모든 문제를 제출했는지 확인
      List<StudySessionItem> allReviewItems = sessionManager.items(session.getId());
      long reviewAnsweredCount = allReviewItems.stream()
          .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
          .count();
      boolean allQuestionsAnswered = reviewAnsweredCount >= REVIEW_SIZE && 
                                     reviewAnsweredCount == allReviewItems.size() &&
                                     allReviewItems.size() >= REVIEW_SIZE;
      
      // mcqMeta에서 정보 가져오기 (REVIEW는 MCQ와 동일한 메타데이터 구조 사용)
      double scorePctFromMeta = mcqTotal > 0 ? (mcqCorrect * 100.0) / mcqTotal : 0.0;
      boolean passedFromMeta = mcqCorrect == mcqTotal && mcqTotal > 0;
      
      // scorePctFromMeta를 mcqMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 더 정확)
      Object scorePctObj = mcqMeta.get("scorePct");
      if (scorePctObj != null && scorePctObj instanceof Number) {
        double metaScorePct = ((Number) scorePctObj).doubleValue();
        // summary_json에 값이 있으면 그것을 사용 (더 신뢰할 수 있음)
        scorePctFromMeta = metaScorePct;
      }
      
      // score_pct 업데이트 필요 여부 확인 (0점도 포함하여 항상 업데이트)
      boolean needsUpdate = session.getScorePct() == null || 
                            Math.abs(session.getScorePct() - scorePctFromMeta) > 0.01 ||
                            !Boolean.TRUE.equals(session.getPassed()) != !passedFromMeta ||
                            !"CLOSED".equals(session.getStatus());
      
      if (needsUpdate) {
        Map<String, Object> currentMeta = sessionManager.loadMeta(session);
        
        // 세션이 완료되었거나 모든 문제를 풀었으면 CLOSED로 변경
        if (sessionFinished || sessionCompleted || completed || allQuestionsAnswered) {
          System.out.println("[WrittenService.reviewSummary] Updating session: sessionId=" + session.getId() + 
                            ", scorePct=" + session.getScorePct() + " -> " + scorePctFromMeta + 
                            ", passed=" + session.getPassed() + " -> " + passedFromMeta);
          sessionManager.closeSession(session, scorePctFromMeta, passedFromMeta, currentMeta);
        }
      }
    }

    // SUMMARY 단계는 advance API를 통해 완료 처리되어야 함
    // 상태 변경은 advance에서 수행되므로 여기서는 하지 않음
    
    // 최종 payload 생성 (XP 정보 포함)
    WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
        0,  // Review 모드에는 MINI 없음
        0,
        false,
        mcqTotal,
        mcqCorrect,
        0,
        0,
        false,
        summaryText,
        completed,
        earnedXp,
        totalXp,
        level,
        xpToNextLevel,
        leveledUp,
        levelUpRewardPoints
    );
    
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
    
    // 2. 모드에 따라 적절한 단계 선택
    String mcqStepName;
    String miniStepName;
    
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(learningSession.getMode())) {
      // difficulty 기반 보조학습: MINI 단계가 없고 ASSIST_WRITTEN_DIFFICULTY 단계만 있음
      mcqStepName = "ASSIST_WRITTEN_DIFFICULTY";
      miniStepName = null;
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(learningSession.getMode())) {
      // weakness 기반 보조학습: MINI 단계가 없고 ASSIST_WRITTEN_WEAKNESS 단계만 있음
      mcqStepName = "ASSIST_WRITTEN_WEAKNESS";
      miniStepName = null;
    } else if ("ASSIST_WRITTEN_CATEGORY".equals(learningSession.getMode())) {
      // category 기반 보조학습: MINI 단계가 없고 ASSIST_WRITTEN_CATEGORY 단계만 있음
      mcqStepName = "ASSIST_WRITTEN_CATEGORY";
      miniStepName = null;
    } else {
      // 일반 학습: MINI와 MCQ 단계 사용
      mcqStepName = "MCQ";
      miniStepName = "MINI";
    }
    
    // 3. StudySession 조회
    LearningStep mcqStep = learningSessionService.getStep(learningSession, mcqStepName);
    StudySession session = mcqStep.getStudySession();

    // 4. 약점 태그 계산
    List<String> weakTags = List.of();
    Map<String, Object> meta = Map.of();
    Long sessionId = null;
    
    if (session != null) {
      sessionId = session.getId();
      meta = sessionManager.loadMeta(session);
      
      // summary_json에서 직접 정보 가져오기 (이미 올바른 정보가 있음)
      // summary_json이 더 신뢰할 수 있는 소스임
    }
    
    // 4-1. LearningStep과 summary_json에서 메타데이터 추출 (summary_json 우선)
    Map<String, Object> miniMeta;
    Map<String, Object> mcqMeta = parseJson(mcqStep.getMetadataJson());
    
    // summary_json에서 mcq 정보 가져오기 (우선순위 높음)
    if (session != null && !meta.isEmpty()) {
      Object mcqRaw = meta.get("mcq");
      if (mcqRaw instanceof Map<?, ?> mcqFromSummary) {
        Map<String, Object> mcqFromSummaryMap = new HashMap<>();
        mcqFromSummary.forEach((k, v) -> mcqFromSummaryMap.put(String.valueOf(k), v));
        // summary_json의 정보로 덮어쓰기
        mcqMeta.putAll(mcqFromSummaryMap);
      }
    }
    
    int miniTotal = 0;
    int miniCorrect = 0;
    boolean miniPassed = true; // difficulty 기반 보조학습은 MINI가 없으므로 항상 통과로 간주
    
    if (miniStepName != null) {
      LearningStep miniStep = learningSessionService.getStep(learningSession, miniStepName);
      miniMeta = parseJson(miniStep.getMetadataJson());
      
      // summary_json에서 mini 정보 가져오기 (우선순위 높음)
      if (session != null && !meta.isEmpty()) {
        Object miniRaw = meta.get("mini");
        if (miniRaw instanceof Map<?, ?> miniFromSummary) {
          Map<String, Object> miniFromSummaryMap = new HashMap<>();
          miniFromSummary.forEach((k, v) -> miniFromSummaryMap.put(String.valueOf(k), v));
          // summary_json의 정보로 덮어쓰기
          miniMeta.putAll(miniFromSummaryMap);
        }
      }
      
      miniTotal = readInt(miniMeta, "total");
      miniCorrect = readInt(miniMeta, "correct");
      miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
    } else {
      miniMeta = Map.of();
    }
    
    int mcqTotal = readInt(mcqMeta, "total");
    int mcqCorrect = readInt(mcqMeta, "correct");
    boolean mcqCompleted = Boolean.TRUE.equals(mcqMeta.get("completed"));
    
    // 약점 태그 계산
    if (session != null && sessionId != null) {
      List<UserAnswer> sessionAnswers = userAnswerRepository.findByUserIdAndSessionId(userId, sessionId).stream()
          .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
          .toList();
      Set<Long> questionIds = sessionAnswers.stream().map(UserAnswer::getQuestionId).collect(Collectors.toSet());
      Map<Long, Question> questionCache = questionRepository.findByIdIn(questionIds).stream()
          .filter(q -> topicId == 0L || Objects.equals(q.getTopicId(), topicId)) // difficulty 기반 보조학습은 topicId=0
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
    if (topicId != 0L) { // difficulty/category 기반 보조학습(topicId=0)은 토픽 제목 조회 생략
      try {
        CurriculumGateway.CurriculumConcept curriculum = curriculumGateway.getConceptWithTopic(topicId);
        topicTitle = curriculum.topicTitle();
      } catch (Exception ignored) {
      }
    } else {
      if ("ASSIST_WRITTEN_CATEGORY".equals(learningSession.getMode())) {
        topicTitle = "카테고리 기반 보조학습";
      } else {
        topicTitle = "난이도 기반 보조학습";
      }
    }

    String summaryText = aiExplanationService.summarizeWritten(
        topicTitle,
        totalSolved,
        totalCorrect,
        weakTags
    );

    // XP 정보 초기값 (완료되지 않았거나 XP 지급 조건 미충족 시 null)
    Integer earnedXp = null;
    Long totalXp = null;
    Integer level = null;
    Integer xpToNextLevel = null;
    Boolean leveledUp = null;
    Integer levelUpRewardPoints = null;

    String status;
    if (learningSession == null) {
      status = "NOT_STARTED";
    } else {
      status = completed ? "COMPLETE" : "IN_PROGRESS";
    }

    // MICRO 모드: MINI와 MCQ 모두 완료되었는지 확인
    boolean trulyCompleted = learningSession != null && Boolean.TRUE.equals(learningSession.getTrulyCompleted());
    boolean miniAllPassed = false;
    
    if ("MICRO".equals(learningSession != null ? learningSession.getMode() : null)) {
      // MINI가 모두 맞았는지 확인
      miniAllPassed = miniPassed && miniTotal >= MINI_SIZE && miniCorrect == miniTotal;
      // MCQ도 모두 맞았는지 확인
      boolean mcqAllCorrect = mcqCompleted && mcqTotal >= MCQ_SIZE && mcqCorrect == mcqTotal;
      
      // XP 지급 (xp_granted=0이면 항상 지급, 정답률 기반)
      // 조건: xp_granted=0 (passed와 상관없이 정답률에 따라 XP 지급)
      // MICRO는 MINI와 MCQ 모두 완료되었을 때 XP 지급
      if (sessionId != null && session != null && !Boolean.TRUE.equals(session.getXpGranted())) {
        try {
          // 정답률 계산 (MICRO는 MINI+MCQ 합산 정답률 사용)
          // 1순위: mcqMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 가장 정확)
          double scorePct;
          Object scorePctObj = mcqMeta.get("scorePct");
          if (scorePctObj != null && scorePctObj instanceof Number) {
            double mcqScorePct = ((Number) scorePctObj).doubleValue();
            // MICRO는 MINI + MCQ 합산 정답률 사용
            if (totalSolved > 0) {
              scorePct = (totalCorrect * 100.0) / totalSolved;
            } else {
              scorePct = mcqScorePct; // MCQ만 있는 경우
            }
            System.out.println("[WrittenService.summary] scorePct from mcqMeta: " + mcqScorePct + 
                             ", calculated total: " + scorePct + " (totalSolved=" + totalSolved + ", totalCorrect=" + totalCorrect + ")");
          } else if (totalSolved > 0) {
            // 2순위: 계산하기
            scorePct = (totalCorrect * 100.0) / totalSolved;
            System.out.println("[WrittenService.summary] scorePct calculated: " + scorePct + 
                            " (totalSolved=" + totalSolved + ", totalCorrect=" + totalCorrect + ")");
          } else if (session.getScorePct() != null && session.getScorePct() > 0.0) {
            // 3순위: session에서 가져오기 (0이 아닐 때만)
            scorePct = session.getScorePct();
            System.out.println("[WrittenService.summary] scorePct from session: " + scorePct);
          } else {
            // 4순위: 기본값 0.0
            scorePct = 0.0;
            System.out.println("[WrittenService.summary] scorePct default: 0.0");
          }
          
          // scorePct는 반드시 0.0 이상 100.0 이하의 유효한 값이어야 함
          if (scorePct < 0.0 || scorePct > 100.0 || Double.isNaN(scorePct)) {
            System.err.println("[WrittenService.summary] Invalid scorePct: " + scorePct + ", using 0.0");
            scorePct = 0.0;
          }
          
          System.out.println("[WrittenService.summary] XP 지급 요청 (MICRO): sessionId=" + session.getId() + 
                            ", totalSolved=" + totalSolved + ", totalCorrect=" + totalCorrect + 
                            ", scorePct=" + scorePct);
          
          // 1. XP 지급 요청 (정답률 기반) - scorePct는 절대 null이 아님
          ProgressXpClient.XpEarnRequest xpRequest = new ProgressXpClient.XpEarnRequest(
              "WRITTEN_MICRO",
              session.getId(),
              topicId,
              scorePct  // 항상 유효한 Double 값 (null 아님)
          );
          
          System.out.println("[WrittenService.summary] XP 요청 상세: activityType=" + xpRequest.activityType() + 
                            ", sessionId=" + xpRequest.sessionId() + ", topicId=" + xpRequest.topicId() + 
                            ", scorePct=" + xpRequest.scorePct());
          
          ProgressXpClient.XpEarnResponse xpResp = progressXpClient.earnXp(xpRequest);
          
          // 2. XP 정보를 응답에 포함
          earnedXp = xpResp.earnedXp();
          totalXp = xpResp.totalXp();
          level = xpResp.level();
          xpToNextLevel = xpResp.xpToNextLevel();
          leveledUp = xpResp.leveledUp();
          levelUpRewardPoints = xpResp.levelUpRewardPoints();
          
          // 3. xp_granted 플래그 업데이트
          sessionManager.markXpGranted(session);
          // MINI 세션도 표시 (MICRO는 MINI+MCQ 합쳐서 하나의 XP)
          LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
          StudySession miniSession = miniStep != null ? miniStep.getStudySession() : null;
          if (miniSession != null && !Boolean.TRUE.equals(miniSession.getXpGranted())) {
            sessionManager.markXpGranted(miniSession);
          }
          
          // 4. 기존 hook도 호출 (다른 통계 처리용)
          try {
            progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
                userId,
                ExamMode.WRITTEN.name(),
                "MICRO",
                topicId,
                scorePct  // 정답률 전달
            ));
          } catch (Exception hookEx) {
            // hook 실패는 학습 흐름을 막지 않음
            System.err.println("Failed to call flow-complete hook: " + hookEx.getMessage());
          }
          
        } catch (Exception e) {
          // XP 지급 실패는 학습 흐름을 막지 않음, 로깅만 수행
          System.err.println("Failed to grant XP in getSummary (MICRO): " + e.getMessage());
          e.printStackTrace();
        }
      }
    }
    
    // MCQ 세션의 score_pct와 passed 업데이트 (모든 모드에 대해)
    // summary_json에 완료 정보가 있으면 그것을 사용해서 직접 업데이트
    if (session != null && mcqTotal > 0) {
        // summary_json의 정보를 사용해서 score_pct와 passed 업데이트
        double scorePctFromSummary = mcqTotal > 0 ? (mcqCorrect * 100.0) / mcqTotal : 0.0;
        boolean passedFromSummary = mcqCorrect == mcqTotal && mcqTotal > 0;
        
        // scorePctFromSummary를 mcqMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 더 정확)
        Object scorePctObj = mcqMeta.get("scorePct");
        if (scorePctObj != null && scorePctObj instanceof Number) {
          double metaScorePct = ((Number) scorePctObj).doubleValue();
          // summary_json에 값이 있으면 그것을 사용 (더 신뢰할 수 있음)
          scorePctFromSummary = metaScorePct;
        }
        
        // score_pct 업데이트 필요 여부 확인 (0점도 포함하여 항상 업데이트)
        boolean needsUpdate = session.getScorePct() == null || 
                              Math.abs(session.getScorePct() - scorePctFromSummary) > 0.01 ||
                              !Boolean.TRUE.equals(session.getPassed()) != !passedFromSummary ||
                              !"CLOSED".equals(session.getStatus());
        
        if (needsUpdate) {
          System.out.println("[WrittenService.summary] Updating MCQ session: sessionId=" + session.getId() + 
                             ", scorePct=" + session.getScorePct() + " -> " + scorePctFromSummary + 
                             ", passed=" + session.getPassed() + " -> " + passedFromSummary);
          
          Map<String, Object> currentMeta = sessionManager.loadMeta(session);
          
          // 세션이 완료되었거나 모든 문제를 풀었으면 CLOSED로 변경
          boolean sessionFinished = session.getFinishedAt() != null;
          boolean sessionCompleted = Boolean.TRUE.equals(session.getCompleted());
          boolean allQuestionsAnswered = mcqCompleted || (mcqTotal >= MCQ_SIZE && mcqCorrect + (mcqTotal - mcqCorrect) == mcqTotal);
          
          if (sessionFinished || sessionCompleted || mcqCompleted || allQuestionsAnswered) {
            sessionManager.closeSession(session, scorePctFromSummary, passedFromSummary, currentMeta);
          }
          
          System.out.println("[WrittenService.summary] After update: sessionId=" + session.getId() + 
                             ", scorePct=" + session.getScorePct() + ", passed=" + session.getPassed());
        }
    }

    // SUMMARY 단계는 advance API를 통해 완료 처리되어야 함
    // 상태 변경은 advance에서 수행되므로 여기서는 하지 않음
    
    // 최종 payload 생성 (XP 정보 포함)
    WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
        miniTotal,
        miniCorrect,
        miniPassed,
        mcqTotal,
        mcqCorrect,
        0,
        0,
        false,
        summaryText,
        completed,
        earnedXp,
        totalXp,
        level,
        xpToNextLevel,
        leveledUp,
        levelUpRewardPoints
    );
    
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
    String stepName;
    
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(learningSession.getMode())) {
      // difficulty 기반 보조학습
      stepName = "ASSIST_WRITTEN_DIFFICULTY";
      stepCode = "ASSIST_WRITTEN_DIFFICULTY";
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(learningSession.getMode())) {
      // weakness 기반 보조학습
      stepName = "ASSIST_WRITTEN_WEAKNESS";
      stepCode = "ASSIST_WRITTEN_WEAKNESS";
    } else if ("ASSIST_WRITTEN_CATEGORY".equals(learningSession.getMode())) {
      // category 기반 보조학습
      stepName = "ASSIST_WRITTEN_CATEGORY";
      stepCode = "ASSIST_WRITTEN_CATEGORY";
    } else if ("REVIEW".equals(learningSession.getMode())) {
      stepName = "MCQ";
      stepCode = "REVIEW_MCQ";
    } else {
      stepName = "MCQ";
      stepCode = "MICRO_MCQ";
    }
    
    // 해당 단계의 LearningStep 조회
    LearningStep step = learningSessionService.getStep(learningSession, stepName);
    
    // StudySession 조회
    StudySession session = step.getStudySession();
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

    // 7. 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    // study_session_item 기준으로 실제 제출된 문제 수 확인
    List<StudySessionItem> allItems = sessionManager.items(session.getId());
    long answeredCount = allItems.stream()
        .filter(sessionItem -> sessionItem.getUserAnswerJson() != null && !sessionItem.getUserAnswerJson().isBlank())
        .count();
    boolean allQuestionsAnswered = answeredCount >= MINI_SIZE && answeredCount == allItems.size();
    
    // 8. 세션이 완료되었을 때 finalizeStudySession 호출
    if (allQuestionsAnswered && session != null && !Boolean.TRUE.equals(session.getCompleted())) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      
      // LearningStep 메타데이터에 wrongQuestionIds 추가 (하위 호환성)
      miniMeta.put("total", result.total());
      miniMeta.put("correct", result.correct());
      miniMeta.put("scorePct", (int) Math.round(result.scorePct()));
      miniMeta.put("passed", result.passed());
      miniMeta.put("completed", true);
      miniMeta.put("wrongQuestionIds", allWrongIds);
      miniStep.setMetadataJson(toJson(miniMeta));
      miniStep.setScorePct((int) Math.round(result.scorePct()));
      miniStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(miniStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      miniStep.setMetadataJson(metadataJson);
      miniStep.setScorePct(accumulatedScorePct);
      miniStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(miniStep);
    }

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

    // 6. 진정한 완료 설정 (MCQ 완료 시 - 모든 문제를 맞춰야 완료)
    if (finalCompleted && allWrongIds.isEmpty() && learningSession.getTrulyCompleted() == null) {
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

    // 태그 정보 조회
    List<com.OhRyue.common.dto.TagViewDto> tags = getTagsByQuestionIds(List.of(questionId))
        .getOrDefault(questionId, List.of());

    return new WrittenDtos.QuestionDetailResp(
        question.getId(),
        type,
        stem,
        choices,
        correctAnswer,
        explanation,
        tags
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

    // 태그 정보 조회
    Map<Long, List<com.OhRyue.common.dto.TagViewDto>> tagsByQuestionId = getTagsByQuestionIds(uniqueIds);

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

          List<com.OhRyue.common.dto.TagViewDto> tags = tagsByQuestionId.getOrDefault(question.getId(), List.of());

          return new WrittenDtos.QuestionDetailResp(
              question.getId(),
              type,
              stem,
              choices,
              correctAnswer,
              explanation,
              tags
          );
        })
        .toList();

    return new WrittenDtos.QuestionDetailListResp(results);
  }

  /* ========================= 내부 유틸 ========================= */

  /**
   * 문제 ID 목록으로 태그 정보 조회 (TagViewDto 포함)
   */
  private Map<Long, List<com.OhRyue.common.dto.TagViewDto>> getTagsByQuestionIds(Collection<Long> questionIds) {
    return tagQueryService.getTagsByQuestionIds(questionIds, questionTagRepository);
  }

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
      case "ASSIST_WRITTEN_DIFFICULTY", "ASSIST_WRITTEN_WEAKNESS", "ASSIST_WRITTEN_CATEGORY" -> "ASSIST_WRITTEN"; // 보조학습 (difficulty/weakness/category)
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

  /**
   * [C] REVIEW 완료 체크 및 XP 지급
   * REVIEW 세션이 완료되고 모든 문제를 맞았는지 확인하고 XP 지급
   */
  private void checkReviewCompletionAndXp(LearningSession learningSession, StudySession reviewSession, ExamMode examMode) {
    if (!"REVIEW".equals(learningSession.getMode())) {
      return; // REVIEW 모드가 아니면 체크하지 않음
    }

    String userId = learningSession.getUserId();
    
    // REVIEW 세션이 완료되었는지 확인
    if (reviewSession == null || !Boolean.TRUE.equals(reviewSession.getCompleted()) ||
        reviewSession.getScorePct() == null) {
      return;
    }

    // REVIEW 모드에서는 study_session의 passed가 1이어야만 truly_completed를 1로 설정
    if (Boolean.TRUE.equals(reviewSession.getPassed()) && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    } else if (!Boolean.TRUE.equals(reviewSession.getPassed()) && Boolean.TRUE.equals(learningSession.getTrulyCompleted())) {
      // passed가 0인데 truly_completed가 1인 경우, 0으로 되돌림
      learningSession.setTrulyCompleted(false);
      learningSessionService.saveLearningSession(learningSession);
    }

    // XP 지급 (idempotent, xp_granted만 체크)
    // summary 메서드에서 이미 처리하므로 여기서는 제거 (또는 summary에서 호출하지 않음)
    // 이 메서드는 더 이상 사용하지 않거나, summary에서만 XP 지급하도록 변경
  }

  /**
   * [B] MICRO 완료 체크 및 XP 지급
   * MINI, MCQ, SUMMARY 모두 완료되었는지 확인하고 XP 지급
   */
  private void checkMicroCompletionAndXp(LearningSession learningSession, StudySession mcqSession, ExamMode examMode) {
    if (!"MICRO".equals(learningSession.getMode())) {
      return; // MICRO 모드가 아니면 체크하지 않음
    }

    // summary 메서드에서 이미 XP 지급 로직이 처리되므로, 여기서는 제거
    // 이 메서드는 더 이상 사용하지 않거나, summary에서만 XP 지급하도록 변경
    // 또는 세션이 완료되었는지만 체크하고 XP는 summary에서 처리
  }
}
