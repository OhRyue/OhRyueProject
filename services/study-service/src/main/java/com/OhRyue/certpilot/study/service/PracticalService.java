package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
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
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.UserAnswerRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.NoSuchElementException;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class PracticalService {

  private static final int MINI_SIZE = 4;
  private static final int PRACTICAL_SIZE = 5;
  private static final int REVIEW_SIZE = 10;

  private final QuestionRepository questionRepository;
  private final QuestionTagRepository questionTagRepository;
  private final UserAnswerRepository userAnswerRepository;
  private final UserProgressRepository userProgressRepository;
  private final StudySessionItemRepository studySessionItemRepository;
  private final LearningStepRepository learningStepRepository;
  private final StudySessionManager sessionManager;
  private final LearningSessionService learningSessionService;
  private final AIExplanationService aiExplanationService;
  private final TopicTreeService topicTreeService;
  private final ProgressHookClient progressHookClient;
  private final ObjectMapper objectMapper;

  // cert-service 커리큘럼 연동 (토픽 제목/개념 조회용)
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
   */
  @Transactional
  public void completeConcept(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    LearningStep conceptStep = learningSessionService.getStep(learningSession, "CONCEPT");
    learningSessionService.updateStepStatus(conceptStep, "COMPLETE", null, null);
    
    // MINI 단계를 IN_PROGRESS로 변경
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    if ("READY".equals(miniStep.getStatus())) {
      learningSessionService.updateStepStatus(miniStep, "IN_PROGRESS", null, null);
    }
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
    
    // MINI 단계를 IN_PROGRESS로 변경
    if ("READY".equals(status)) {
      learningSessionService.updateStepStatus(miniStep, "IN_PROGRESS", null, null);
      status = "IN_PROGRESS";
    }

    // 미니 정답 여부와 관계없이 다음 단계는 항상 PRACTICAL_SET 로 이동 가능
    String next = completed ? "PRACTICAL_SET" : null;

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "PRACTICAL",
        "PRACTICAL_MINI",
        completed ? "COMPLETE" : "IN_PROGRESS",
        next,
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

    Map<Long, Question> questionMap = questionRepository.findByIdIn(
            req.answers().stream().map(WrittenDtos.MiniAnswer::questionId).toList())
        .stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL && q.getType() == QuestionType.OX)
        .collect(Collectors.toMap(Question::getId, q -> q));
    
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

      persistUserAnswer(userId, question, userAnswer, isCorrect, 100, session, item, "PRACTICAL_MINI");
      pushProgressHook(userId, QuestionType.OX, isCorrect, 100, question.getId());
      updateProgress(userId, question.getTopicId(), isCorrect);
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

    // 5. 미니체크 완료 시 LearningStep 업데이트
    if (newTotal >= MINI_SIZE) {
      learningSessionService.updateStepStatus(miniStep, "COMPLETE", accumulatedScorePct, metadataJson);
      // PRACTICAL 단계를 IN_PROGRESS로 변경
      LearningStep practicalStep = learningSessionService.getStep(learningSession, "PRACTICAL");
      if (practicalStep != null && "READY".equals(practicalStep.getStatus())) {
        learningSessionService.updateStepStatus(practicalStep, "IN_PROGRESS", null, null);
      }
    } else {
      // 미완료 시 상태만 업데이트
      learningSessionService.updateStepStatus(miniStep, "IN_PROGRESS", accumulatedScorePct, metadataJson);
    }

    boolean finalPassed = everPassed;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "PRACTICAL_MINI",
        finalPassed ? "COMPLETE" : "IN_PROGRESS",
        // 항상 PRACTICAL_SET 으로 이동 가능하도록 고정
        "PRACTICAL_SET",
        sessionManager.loadMeta(session),
        new WrittenDtos.MiniSubmitResp(req.answers().size(), correctCount, finalPassed, resultItems, wrongQuestionIds),
        learningSession.getId()
    );
  }

  public WrittenDtos.MiniGradeOneResp gradeOneMini(Long learningSessionId, WrittenDtos.MiniGradeOneReq req) {
    FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> envelope =
        submitMini(learningSessionId, new WrittenDtos.MiniSubmitReq(
            req.topicId(),
            List.of(new WrittenDtos.MiniAnswer(req.questionId(), req.answer()))
        ));

    WrittenDtos.MiniSubmitResp resp = envelope.payload();
    WrittenDtos.MiniSubmitItem item = resp.items().isEmpty()
        ? new WrittenDtos.MiniSubmitItem(req.questionId(), false, "", "")
        : resp.items().get(0);

    return new WrittenDtos.MiniGradeOneResp(
        item.correct(), 
        item.explanation(),
        envelope.learningSessionId()
    );
  }

  /* ========================= 실기 세트 (Micro) ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalSet> practicalSet(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }

    // 2. PRACTICAL 단계 조회 (없으면 예외 발생)
    LearningStep practicalStep;
    try {
      practicalStep = learningSessionService.getStep(learningSession, "PRACTICAL");
    } catch (NoSuchElementException e) {
      throw new IllegalStateException("PRACTICAL 단계가 없습니다. 세션을 다시 시작해주세요. (이전 버전의 세션일 수 있습니다)", e);
    }
    
    // 3. MINI 단계에서 StudySession 가져오기 (실기는 MINI와 PRACTICAL이 같은 StudySession 공유)
    LearningStep miniStep;
    try {
      miniStep = learningSessionService.getStep(learningSession, "MINI");
    } catch (NoSuchElementException e) {
      throw new IllegalStateException("MINI 단계가 없습니다. 세션을 다시 시작해주세요.", e);
    }
    
    StudySession session = miniStep.getStudySession();
    
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다. MINI 단계를 먼저 완료해주세요.");
    }

    // 4. PRACTICAL 단계 상태 확인
    String stepStatus = practicalStep.getStatus();
    boolean completed = "COMPLETE".equals(stepStatus);
    
    // PRACTICAL 단계를 IN_PROGRESS로 변경
    if ("READY".equals(stepStatus)) {
      learningSessionService.updateStepStatus(practicalStep, "IN_PROGRESS", null, null);
      stepStatus = "IN_PROGRESS";
    }

    // 세션에 할당된 문제 조회
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    
    // PRACTICAL 단계 문제만 필터링 (MINI 단계 문제 제외)
    List<StudySessionItem> practicalItems = sessionItems.stream()
        .filter(item -> {
          // orderNo가 MINI_SIZE(4)보다 큰 문제만 (실기 세트 문제)
          return item.getOrderNo() > MINI_SIZE;
        })
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .toList();

    List<Long> questionIds;
    
    if (practicalItems.isEmpty()) {
      // 할당된 문제가 없으면 새로 할당 (PRACTICAL 단계에 연결된 StudySession 사용)
      // 실기는 MINI와 PRACTICAL이 같은 StudySession을 공유하므로 MINI 세션 사용
      // SHORT 3 + LONG 2 = 총 5문제
      List<Question> shortQuestions = questionRepository.pickRandomByTopic(
          topicId, ExamMode.PRACTICAL, QuestionType.SHORT, PageRequest.of(0, 3));
      List<Question> longQuestions = questionRepository.pickRandomByTopic(
          topicId, ExamMode.PRACTICAL, QuestionType.LONG, PageRequest.of(0, 2));

      List<Question> combined = Stream.concat(shortQuestions.stream(), longQuestions.stream())
          .distinct()
          .toList();

      if (combined.isEmpty()) {
        throw new IllegalStateException("문제가 부족합니다. topicId: " + topicId);
      }

      // MINI 단계 문제의 개수 확인
      List<StudySessionItem> miniItems = sessionItems.stream()
          .filter(item -> item.getOrderNo() <= MINI_SIZE)
          .toList();
      int baseOrder = miniItems.size();

      // 실기 문제 할당
      List<Long> practicalQuestionIds = combined.stream().map(Question::getId).toList();
      // 직접 할당 (MINI 문제가 이미 있을 수 있으므로 baseOrder부터 시작)
      for (int i = 0; i < practicalQuestionIds.size(); i++) {
        final Long questionId = practicalQuestionIds.get(i);
        final int finalI = i;
        final int finalOrderNo = baseOrder + finalI + 1;
        // 이미 할당된 문제인지 확인
        boolean exists = sessionItems.stream()
            .anyMatch(item -> item.getQuestionId().equals(questionId) && item.getOrderNo() == finalOrderNo);
        if (!exists) {
          // 새로 생성 (답변은 아직 없음)
          studySessionItemRepository.save(StudySessionItem.builder()
              .sessionId(session.getId())
              .questionId(questionId)
              .orderNo(finalOrderNo)
              .userAnswerJson(null)
              .correct(null)
              .score(null)
              .createdAt(Instant.now())
              .build());
        }
      }

      questionIds = practicalQuestionIds;
    } else {
      // 이미 할당된 문제 사용
      questionIds = practicalItems.stream()
          .map(StudySessionItem::getQuestionId)
          .toList();
    }

    // 문제 상세 정보 조회
    if (questionIds.isEmpty()) {
      throw new IllegalStateException("실기 문제가 할당되지 않았습니다. topicId: " + topicId);
    }
    
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 순서대로 문제 반환
    List<PracticalDtos.PracticalQuestion> items;
    if (practicalItems.isEmpty()) {
      // 새로 할당한 경우: questionIds 순서대로
      items = questionIds.stream()
          .map(qId -> questionMap.get(qId))
          .filter(Objects::nonNull)
          .map(q -> new PracticalDtos.PracticalQuestion(
              q.getId(),
              q.getType().name(),
              Optional.ofNullable(q.getStem()).orElse(""),
              q.getImageUrl()))
          .toList();
    } else {
      // 이미 할당된 경우: practicalItems 순서대로
      items = practicalItems.stream()
          .map(item -> questionMap.get(item.getQuestionId()))
          .filter(Objects::nonNull)
          .map(q -> new PracticalDtos.PracticalQuestion(
              q.getId(),
              q.getType().name(),
              Optional.ofNullable(q.getStem()).orElse(""),
              q.getImageUrl()))
          .toList();
    }
    
    if (items.isEmpty()) {
      throw new IllegalStateException("실기 문제를 찾을 수 없습니다. topicId: " + topicId + ", questionIds: " + questionIds);
    }

    // 미니 통과 여부와 상관없이 세트 진입 허용
    Map<String, Object> practicalMeta = sessionManager.loadStepMeta(session, "practical");
    boolean metaCompleted = Boolean.TRUE.equals(practicalMeta.get("completed"));
    completed = completed || metaCompleted;
    String status = completed ? "COMPLETE" : "IN_PROGRESS";
    String next = completed ? "PRACTICAL_SUMMARY" : null;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "PRACTICAL_SET",
        status,
        next,
        sessionManager.loadMeta(session),
        new PracticalDtos.PracticalSet(items),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalSubmitResp> submitPractical(Long learningSessionId, PracticalDtos.PracticalSubmitReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.topicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    
    // 2. MINI 단계에서 StudySession 가져오기 (실기는 MINI와 PRACTICAL이 같은 StudySession 공유)
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    StudySession session = miniStep.getStudySession();
    
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // PRACTICAL 단계 문제만
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());

    for (PracticalDtos.PracticalAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }

    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // PRACTICAL 단계 문제만
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));

    Map<Long, Question> questionMap = questionRepository.findByIdIn(
            req.answers().stream().map(PracticalDtos.PracticalAnswer::questionId).toList())
        .stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    List<PracticalDtos.PracticalSubmitItem> items = new ArrayList<>();
    List<Long> wrongIds = new ArrayList<>();
    int correctCount = 0;

    for (int idx = 0; idx < req.answers().size(); idx++) {
      PracticalDtos.PracticalAnswer answer = req.answers().get(idx);
      Question question = questionMap.get(answer.questionId());
      if (question == null || !isPractical(question)) {
        throw new NoSuchElementException("Invalid practical question: " + answer.questionId());
      }

      AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
          question, answer.userText());
      boolean correct = Optional.ofNullable(result.correct()).orElse(false);
      if (correct) {
        correctCount++;
      } else {
        wrongIds.add(question.getId());
      }

      items.add(new PracticalDtos.PracticalSubmitItem(
          question.getId(),
          correct,
          Optional.ofNullable(question.getSolutionText()).orElse(""),
          result.explain()
      ));

      Map<String, Object> answerJson = new HashMap<>();
      answerJson.put("answer", Optional.ofNullable(answer.userText()).orElse(""));
      answerJson.put("correct", correct);
      answerJson.put("tips", result.tips());

      // 순서는 세션에 할당된 순서 사용
      int orderNo = questionOrderMap.get(question.getId());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
          toJson(answerJson),
          correct,
          correct ? 100 : 0,  // 하위 호환성을 위해 점수도 저장 (맞으면 100, 틀리면 0)
          toJson(Map.of("explain", result.explain(), "tips", result.tips()))
      );

      persistUserAnswer(
          userId,
          question,
          answer.userText(),
          correct,
          correct ? 100 : 0,  // 하위 호환성을 위해 점수도 저장
          session,
          item,
          "MICRO_PRACTICAL"
      );
      pushProgressHook(userId, question.getType(), correct, correct ? 100 : 0, question.getId());
      updateProgress(userId, question.getTopicId(), correct);
    }

    // upsertItem 후 최신 상태로 다시 조회 (트랜잭션 내에서 반영 확인)
    List<StudySessionItem> updatedSessionItems = sessionManager.items(session.getId());
    
    // 할당된 PRACTICAL 문제의 총 개수 확인
    List<StudySessionItem> allPracticalItems = updatedSessionItems.stream()
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // PRACTICAL 단계 문제만
        .toList();
    int allocatedTotal = allPracticalItems.size();
    
    // 모든 문제를 풀었는지 확인 (답변이 있는 문제 수)
    long answeredCount = allPracticalItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allQuestionsAnswered = allocatedTotal > 0 && answeredCount >= allocatedTotal;

    // 4. LearningStep 업데이트
    LearningStep practicalStep = learningSessionService.getStep(learningSession, "PRACTICAL");
    Map<String, Object> prevPracticalMeta = parseJson(practicalStep.getMetadataJson());
    
    // StudySessionItem에서 실제 정답 정보 직접 계산 (누적 계산 대신)
    int total = allocatedTotal;  // 할당된 총 문제 수
    int totalCorrect = 0;
    List<Long> allWrongIds = new ArrayList<>();
    
    for (StudySessionItem item : allPracticalItems) {
      if (Boolean.TRUE.equals(item.getCorrect())) {
        totalCorrect++;
      } else if (Boolean.FALSE.equals(item.getCorrect())) {
        allWrongIds.add(item.getQuestionId());
      }
    }
    
    boolean allPassedNow = total > 0 && allWrongIds.isEmpty();
    boolean everCompleted = Boolean.TRUE.equals(prevPracticalMeta.get("completed"));
    boolean finalCompleted = everCompleted || allQuestionsAnswered;  // 모든 문제를 풀었으면 완료

    Map<String, Object> practicalMeta = new HashMap<>(prevPracticalMeta);
    practicalMeta.put("total", total);
    practicalMeta.put("correct", totalCorrect);
    practicalMeta.put("completed", finalCompleted);
    practicalMeta.put("wrongQuestionIds", allWrongIds);
    practicalMeta.put("lastSubmittedAt", Instant.now().toString());
    
    String metadataJson = toJson(practicalMeta);
    int scorePct = total == 0 ? 0 : (totalCorrect * 100) / total;  // 정확도 퍼센트 (하위 호환성)

    // StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "practical", practicalMeta);

    // PRACTICAL 단계 업데이트: 모든 문제를 풀었으면 COMPLETE
    if (finalCompleted) {
      learningSessionService.updateStepStatus(practicalStep, "COMPLETE", scorePct, metadataJson);
      // 오답이 있으면 REVIEW_WRONG 단계를 IN_PROGRESS로, 없으면 SUMMARY로
      if (!allWrongIds.isEmpty()) {
        // 오답이 있으면 REVIEW_WRONG 단계로 이동
        LearningStep reviewStep = learningSessionService.getStep(learningSession, "REVIEW_WRONG");
        if (reviewStep != null && "READY".equals(reviewStep.getStatus())) {
          learningSessionService.updateStepStatus(reviewStep, "IN_PROGRESS", null, null);
        }
      } else {
        // 오답이 없으면 바로 SUMMARY로
        LearningStep summaryStep = learningSessionService.getStep(learningSession, "SUMMARY");
        if (summaryStep != null && "READY".equals(summaryStep.getStatus())) {
          learningSessionService.updateStepStatus(summaryStep, "IN_PROGRESS", null, null);
        }
      }
    } else {
      learningSessionService.updateStepStatus(practicalStep, "IN_PROGRESS", scorePct, metadataJson);
    }

    // 다음 단계 결정: 오답이 있으면 REVIEW_WRONG, 없으면 SUMMARY
    String nextStep = finalCompleted 
        ? (!allWrongIds.isEmpty() ? "PRACTICAL_REVIEW_WRONG" : "PRACTICAL_SUMMARY")
        : "PRACTICAL_SET";

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "PRACTICAL_SET",
        finalCompleted ? "COMPLETE" : "IN_PROGRESS",
        nextStep,
        sessionManager.loadMeta(session),
        new PracticalDtos.PracticalSubmitResp(
            items.size(),
            correctCount,
            items,
            wrongIds
        ),
        learningSession.getId()
    );
  }

  /* ========================= 실기 리뷰 (Review) ========================= */

  @Transactional
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalSet> practicalReviewSet(Long rootTopicId) {
    String userId = AuthUserUtil.getCurrentUserId();

    StudySession session = sessionManager.ensureReviewSession(
        userId, rootTopicId, ExamMode.PRACTICAL, REVIEW_SIZE);

    // 세션에 할당된 문제 조회
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    List<Long> questionIds;

    if (sessionItems.isEmpty()) {
      // 할당된 문제가 없으면 새로 할당
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

      if (questions.isEmpty()) {
        throw new IllegalStateException("문제가 부족합니다. rootTopicId: " + rootTopicId);
      }

      // 문제 할당
      questionIds = questions.stream().map(Question::getId).toList();
      for (int i = 0; i < questionIds.size(); i++) {
        Long questionId = questionIds.get(i);
        // 새로 생성 (답변은 아직 없음)
        studySessionItemRepository.save(StudySessionItem.builder()
            .sessionId(session.getId())
            .questionId(questionId)
            .orderNo(i + 1)
            .userAnswerJson(null)
            .correct(null)
            .score(null)
            .createdAt(Instant.now())
            .build());
      }
    } else {
      // 이미 할당된 문제 사용
      questionIds = sessionItems.stream()
          .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
          .map(StudySessionItem::getQuestionId)
          .toList();
    }

    // 문제 상세 정보 조회
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 순서대로 문제 반환
    List<PracticalDtos.PracticalQuestion> items = (sessionItems.isEmpty() ?
        questionIds.stream().map(questionMap::get).filter(Objects::nonNull).toList() :
        sessionItems.stream()
            .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
            .map(item -> questionMap.get(item.getQuestionId()))
            .filter(Objects::nonNull)
            .toList())
        .stream()
        .map(q -> new PracticalDtos.PracticalQuestion(
            q.getId(),
            q.getType().name(),
            Optional.ofNullable(q.getStem()).orElse(""),
            q.getImageUrl()))
        .toList();

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
        new PracticalDtos.PracticalSet(items),
        null  // REVIEW는 LearningSession을 사용하지 않음
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalReviewSubmitResp> practicalReviewSubmit(
      PracticalDtos.PracticalReviewSubmitReq req) {

    String userId = AuthUserUtil.getCurrentUserId();

    StudySession session = sessionManager.ensureReviewSession(
        userId, req.rootTopicId(), ExamMode.PRACTICAL, REVIEW_SIZE);

    // 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());

    for (PracticalDtos.PracticalAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }

    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));

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

    List<PracticalDtos.PracticalSubmitItem> items = new ArrayList<>();
    List<Long> wrongIds = new ArrayList<>();
    int correctCount = 0;

    for (int idx = 0; idx < req.answers().size(); idx++) {
      PracticalDtos.PracticalAnswer answer = req.answers().get(idx);
      Question question = questionMap.get(answer.questionId());
      if (question == null) {
        continue;
      }

      AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
          question, answer.userText());
      boolean correct = Optional.ofNullable(result.correct()).orElse(false);
      if (correct) {
        correctCount++;
      } else {
        wrongIds.add(question.getId());
      }

      items.add(new PracticalDtos.PracticalSubmitItem(
          question.getId(),
          correct,
          Optional.ofNullable(question.getSolutionText()).orElse(""),
          result.explain()
      ));

      Map<String, Object> answerJson = new HashMap<>();
      answerJson.put("answer", Optional.ofNullable(answer.userText()).orElse(""));
      answerJson.put("correct", correct);
      answerJson.put("tips", result.tips());

      // 순서는 세션에 할당된 순서 사용
      int orderNo = questionOrderMap.get(question.getId());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
          toJson(answerJson),
          correct,
          correct ? 100 : 0,  // 하위 호환성을 위해 점수도 저장
          toJson(Map.of("explain", result.explain(), "tips", result.tips()))
      );

      persistUserAnswer(
          userId,
          question,
          answer.userText(),
          correct,
          correct ? 100 : 0,  // 하위 호환성을 위해 점수도 저장
          session,
          item,
          "PRACTICAL_REVIEW"
      );
      pushProgressHook(userId, question.getType(), correct, correct ? 100 : 0, question.getId());
      // 실기 리뷰도 Progress 에 반영
      updateProgress(userId, question.getTopicId(), correct);
    }

    int total = items.size();
    boolean allPassedNow = total > 0 && wrongIds.isEmpty();

    // 이전 메타 불러와서 everCompleted 유지
    Map<String, Object> prevReviewMeta = sessionManager.loadStepMeta(session, "review");
    boolean everCompleted = Boolean.TRUE.equals(prevReviewMeta.get("completed"));
    boolean finalCompleted = everCompleted || allPassedNow;

    Map<String, Object> reviewMeta = new HashMap<>(prevReviewMeta);
    reviewMeta.put("total", total);
    reviewMeta.put("correct", correctCount);
    reviewMeta.put("completed", finalCompleted);
    reviewMeta.put("wrongQuestionIds", wrongIds);
    reviewMeta.put("lastSubmittedAt", Instant.now().toString());
    sessionManager.saveStepMeta(session, "review", reviewMeta);

    // 세션 상태: 한 번 COMPLETE 되면 다시 OPEN 으로 돌리지 않음
    // 실기는 맞으면 passed
    int scorePct = total == 0 ? 0 : (correctCount * 100) / total;  // 정확도 퍼센트 (하위 호환성)
    if (!everCompleted && allPassedNow) {
      sessionManager.closeSession(session, scorePct, true, Map.of("correct", correctCount));
    } else if (!everCompleted) {
      sessionManager.updateStatus(session, "OPEN");
    }
    // everCompleted == true 인 경우는 상태 유지

    // Review 세트 완주 시 Flow XP hook (PRACTICAL / REVIEW / rootTopicId)
    // passed=true일 때만 XP 지급, 세션당 1회만
    if (finalCompleted && allPassedNow && !Boolean.TRUE.equals(session.getXpGranted())) {
      try {
        progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
            userId,
            ExamMode.PRACTICAL.name(),
            "REVIEW",
            req.rootTopicId()
        ));
        // XP 지급 성공 시 xpGranted 표시
        sessionManager.markXpGranted(session);
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
            correctCount,
            items,
            wrongIds
        ),
        null  // REVIEW는 LearningSession을 사용하지 않음
    );
  }

  /* ========================= 요약 (Micro Practical Summary) ========================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(Long topicId, Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(topicId)) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }

    // MINI 단계에서 StudySession 가져오기
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    StudySession session = miniStep.getStudySession();

    // 4. LearningStep에서 메타데이터 추출
    LearningStep practicalStep = learningSessionService.getStep(learningSession, "PRACTICAL");

    Map<String, Object> miniMeta = parseJson(miniStep.getMetadataJson());
    Map<String, Object> practicalMeta = parseJson(practicalStep.getMetadataJson());
    
    int miniTotal = readInt(miniMeta, "total");
    int miniCorrect = readInt(miniMeta, "correct");
    boolean miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));

    int practicalTotal = readInt(practicalMeta, "total");
    int practicalCorrect = readInt(practicalMeta, "correct");
    boolean practicalCompleted = Boolean.TRUE.equals(practicalMeta.get("completed"));

    // 5. 약점 태그 계산
    List<String> mistakes = List.of();
    Map<String, Object> meta = Map.of();
    Long sessionId = null;

    if (session != null) {
      sessionId = session.getId();
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
    int practicalPassed = practicalTotal > 0 ? Math.max(0, practicalTotal - readList(practicalMeta, "wrongQuestionIds").size()) : 0;

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
        practicalCorrect,
        mistakes
    );

    WrittenDtos.SummaryResp payload = new WrittenDtos.SummaryResp(
        miniTotal,
        miniCorrect,
        miniPassed,
        practicalTotal,
        practicalCorrect,
        summary,
        completed
    );

    String status = completed ? "COMPLETE" : "IN_PROGRESS";

    // 진정한 완료(PRACTICAL 완료)일 때만 XP 지급
    boolean trulyCompleted = Boolean.TRUE.equals(learningSession.getTrulyCompleted());
    
    if (trulyCompleted && sessionId != null && session != null) {
      if (!Boolean.TRUE.equals(session.getXpGranted())) {
        try {
          progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
              userId,
              ExamMode.PRACTICAL.name(),
              "MICRO",
              topicId
          ));
          sessionManager.markXpGranted(session);
          if (!Boolean.TRUE.equals(session.getCompleted())) {
            int scorePct = practicalTotal == 0 ? 0 : (practicalCorrect * 100) / practicalTotal;
            sessionManager.closeSession(session, scorePct, completed, Map.of());
          }
        } catch (Exception ignored) {
          // XP hook 실패는 학습 흐름을 막지 않음
        }
      }
    }

    // SUMMARY 단계를 COMPLETE로 변경
    LearningStep summaryStep = learningSessionService.getStep(learningSession, "SUMMARY");
    if (!"COMPLETE".equals(summaryStep.getStatus())) {
      learningSessionService.updateStepStatus(summaryStep, "COMPLETE", null, null);
    }
    
    // 모든 단계가 완료되었는지 확인하고, 완료되었으면 LearningSession의 status를 DONE으로 변경
    List<LearningStep> allSteps = learningStepRepository.findByLearningSessionIdOrderByIdAsc(learningSession.getId());
    boolean hasReadyStep = allSteps.stream()
        .anyMatch(step -> "READY".equals(step.getStatus()));
    
    if (!hasReadyStep && !"DONE".equals(learningSession.getStatus())) {
      learningSession.setStatus("DONE");
      learningSession.setUpdatedAt(Instant.now());
      learningSessionService.saveLearningSession(learningSession);
    }

    return new FlowDtos.StepEnvelope<>(
        sessionId,
        "PRACTICAL",
        "PRACTICAL_SUMMARY",
        status,
        null,
        meta,
        payload,
        learningSession.getId()
    );
  }

  public PracticalDtos.PracticalGradeOneResp gradeOnePractical(Long learningSessionId, PracticalDtos.PracticalGradeOneReq req) {
    FlowDtos.StepEnvelope<PracticalDtos.PracticalSubmitResp> envelope =
        submitPractical(learningSessionId, new PracticalDtos.PracticalSubmitReq(
            req.topicId(),
            List.of(new PracticalDtos.PracticalAnswer(req.questionId(), req.userText()))
        ));

    PracticalDtos.PracticalSubmitResp resp = envelope.payload();
    PracticalDtos.PracticalSubmitItem item = resp.items().isEmpty()
        ? new PracticalDtos.PracticalSubmitItem(req.questionId(), false, "", "")
        : resp.items().get(0);

    return new PracticalDtos.PracticalGradeOneResp(
        item.correct(),
        item.baseExplanation(),
        item.aiExplanation()
    );
  }

  /* ========================= Wrong Recap (세션 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapBySession(Long sessionId, String stepCode) {
    String userId = AuthUserUtil.getCurrentUserId();

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

    // StudySessionItem 조회 (AI 해설 포함)
    LinkedHashSet<Long> sessionItemIds = wrongAnswers.stream()
        .map(UserAnswer::getSessionItemId)
        .filter(Objects::nonNull)
        .collect(Collectors.toCollection(LinkedHashSet::new));

    Map<Long, StudySessionItem> sessionItemMap = sessionItemIds.isEmpty()
        ? Map.<Long, StudySessionItem>of()
        : studySessionItemRepository.findAllById(sessionItemIds).stream()
            .collect(Collectors.toMap(StudySessionItem::getId, item -> item));

    List<WrongRecapDtos.WrongRecapSet.Item> items = wrongAnswers.stream()
        .map(ans -> {
          Question q = questionMap.get(ans.getQuestionId());
          if (q == null) return null;
          StudySessionItem sessionItem = ans.getSessionItemId() != null
              ? sessionItemMap.get(ans.getSessionItemId())
              : null;
          return buildWrongRecapItem(q, ans, sessionItem);
        })
        .filter(Objects::nonNull)
        .toList();

    return new WrongRecapDtos.WrongRecapSet(items);
  }

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecapByLearningSession(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    
    // LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    
    // 실기는 MINI 단계와 PRACTICAL 단계가 같은 StudySession을 공유하므로 MINI 단계에서 조회
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    
    // StudySession 조회
    StudySession session = miniStep.getStudySession();
    if (session == null) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }
    
    // 기존 wrongRecapBySession 로직 재사용
    return wrongRecapBySession(session.getId(), "PRACTICAL_SET");
  }

  /* ========================= Wrong Recap (토픽/전체 기준) ========================= */

  @Transactional(readOnly = true)
  public WrongRecapDtos.WrongRecapSet wrongRecap(Long topicId, int limit) {
    String userId = AuthUserUtil.getCurrentUserId();

    List<UserAnswer> wrongAnswers = userAnswerRepository.findByUserId(userId).stream()
        .filter(ans -> ans.getExamMode() == ExamMode.PRACTICAL) // 🔹 실기만
        .filter(ans -> Boolean.FALSE.equals(ans.getCorrect()))
        .sorted(Comparator.comparing(UserAnswer::getAnsweredAt).reversed())
        .toList();

    Set<Long> answerQuestionIds = wrongAnswers.stream()
        .map(UserAnswer::getQuestionId)
        .collect(Collectors.toSet());

    Map<Long, Question> questionCache = questionRepository.findByIdIn(answerQuestionIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
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
  public WrongRecapDtos.WrongRecapSet wrongRecapByIds(String ids) {
    String userId = AuthUserUtil.getCurrentUserId();

    List<Long> questionIds = Arrays.stream(ids.split(","))
        .map(String::trim)
        .filter(s -> !s.isEmpty())
        .map(Long::valueOf)
        .distinct()
        .toList();

    Map<Long, UserAnswer> latestAnswers = latestAnswerMap(userId);

    List<WrongRecapDtos.WrongRecapSet.Item> items = questionRepository.findAllById(questionIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .map(question -> toWrongRecapItem(question, latestAnswers))
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
        .examMode(question.getMode())
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

  private void updateProgress(String userId, Long topicId, boolean correct) {
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
    // 맞으면 100점, 틀리면 0점으로 계산하여 평균 점수 업데이트 (하위 호환성)
    int score = correct ? 100 : 0;
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
        .filter(ans -> Boolean.FALSE.equals(ans.getCorrect()))  // 틀린 답안만
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

  @SuppressWarnings("unchecked")
  private Map<String, Object> parseJson(String json) {
    if (json == null || json.isBlank()) {
      return new HashMap<>();
    }
    try {
      return objectMapper.readValue(json, Map.class);
    } catch (Exception e) {
      return new HashMap<>();
    }
  }

  // 세션/토픽 기반 recap용: Question + UserAnswer 로 recap 아이템 구성
  private WrongRecapDtos.WrongRecapSet.Item buildWrongRecapItem(Question question, UserAnswer answer, StudySessionItem sessionItem) {
    String stem = Optional.ofNullable(question.getStem()).orElse("");
    String baseExplain = Optional.ofNullable(question.getSolutionText()).orElse("");
    String userAnswerJson = (answer == null)
        ? "{}"
        : Optional.ofNullable(answer.getUserAnswerJson()).orElse("{}");

    // AI 해설 파싱 (StudySessionItem의 aiExplainJson에서 가져오기)
    String aiExplanation = "";
    if (sessionItem != null && sessionItem.getAiExplainJson() != null) {
      try {
        Map<String, Object> aiExplainMap = parseJson(sessionItem.getAiExplainJson());
        Object explainObj = aiExplainMap.get("explain");
        if (explainObj != null) {
          aiExplanation = explainObj.toString();
        }
      } catch (Exception e) {
        // 파싱 실패 시 빈 문자열
      }
    }

    // 실기 문제의 정답: answerKey가 있으면 사용, 없으면 빈 문자열
    String correctAnswer = Optional.ofNullable(question.getAnswerKey())
        .map(String::trim)
        .orElse("");
    
    return new WrongRecapDtos.WrongRecapSet.Item(
        question.getId(),
        question.getType().name(),
        stem,
        userAnswerJson,
        correctAnswer,
        baseExplain,
        question.getImageUrl(),
        aiExplanation
    );
  }

  // 오버로드: sessionItem이 없는 경우 (하위 호환성)
  private WrongRecapDtos.WrongRecapSet.Item buildWrongRecapItem(Question question, UserAnswer answer) {
    return buildWrongRecapItem(question, answer, null);
  }

  private WrongRecapDtos.WrongRecapSet.Item toWrongRecapItem(Question question, Map<Long, UserAnswer> latestAnswers) {
    UserAnswer latest = latestAnswers.get(question.getId());
    // 토픽 기준 오답 조회는 sessionItemId가 없을 수 있으므로 null 전달
    return buildWrongRecapItem(question, latest, null);
  }

  private Map<Long, UserAnswer> latestAnswerMap(String userId) {
    return userAnswerRepository.findByUserId(userId).stream()
        .filter(ans -> ans.getExamMode() == ExamMode.PRACTICAL) // 🔹 실기만
        .collect(Collectors.groupingBy(
            UserAnswer::getQuestionId,
            Collectors.collectingAndThen(
                Collectors.maxBy(Comparator.comparing(UserAnswer::getAnsweredAt)),
                opt -> opt.orElse(null)
            )
        ));
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
      default -> stepCode; // 다른 source 를 그대로 사용하고 싶을 때
    };
  }

  /* ========================= ConceptMapper ========================= */

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
}
