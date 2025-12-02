package com.OhRyue.certpilot.study.service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.OhRyue.certpilot.study.client.CurriculumGateway;
import com.OhRyue.certpilot.study.client.ProgressHookClient;
import com.OhRyue.certpilot.study.client.ProgressXpClient;
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
import com.OhRyue.common.auth.AuthUserUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.RequiredArgsConstructor;

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
  private final ProgressXpClient progressXpClient;
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

    // 미니 정답 여부와 관계없이 다음 단계는 항상 SHORT_SET 로 이동 가능
    String next = completed ? "SHORT_SET" : null;

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

    // 5. StudySession 종료 처리
    // 주의: 실기는 MINI와 PRACTICAL이 같은 StudySession을 공유하므로,
    // MINI 완료 시에는 세션을 close하지 않음 (PRACTICAL이 아직 진행 중)
    // PRACTICAL 완료 시에만 세션을 close하고 XP 지급

    // 6. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // MINI 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    miniStep.setMetadataJson(metadataJson);
    miniStep.setScorePct(accumulatedScorePct);
    miniStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(miniStep);

    boolean finalPassed = everPassed;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "PRACTICAL_MINI",
        finalPassed ? "COMPLETE" : "IN_PROGRESS",
        // 항상 SHORT_SET 으로 이동 가능하도록 고정
        "SHORT_SET",
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

    // 2. SHORT 단계 조회 (없으면 예외 발생)
    LearningStep practicalStep;
    try {
      practicalStep = learningSessionService.getStep(learningSession, "SHORT");
    } catch (NoSuchElementException e) {
      throw new IllegalStateException("SHORT 단계가 없습니다. 세션을 다시 시작해주세요. (이전 버전의 세션일 수 있습니다)", e);
    }

    // 3. MINI 단계에서 StudySession 가져오기 (실기는 MINI와 SHORT가 같은 StudySession 공유)
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

    // 4. SHORT 단계 상태 확인
    String stepStatus = practicalStep.getStatus();
    boolean completed = "COMPLETE".equals(stepStatus);

    // SHORT 단계를 IN_PROGRESS로 변경
    if ("READY".equals(stepStatus)) {
      learningSessionService.updateStepStatus(practicalStep, "IN_PROGRESS", null, null);
      stepStatus = "IN_PROGRESS";
    }

    // 세션에 할당된 문제 조회
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());

    // SHORT 단계 문제만 필터링 (MINI 단계 문제 제외)
    List<StudySessionItem> practicalItems = sessionItems.stream()
        .filter(item -> {
          // orderNo가 MINI_SIZE(4)보다 큰 문제만 (실기 세트 문제)
          return item.getOrderNo() > MINI_SIZE;
        })
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .toList();

    List<Long> questionIds;

    if (practicalItems.isEmpty()) {
      // 할당된 문제가 없으면 새로 할당 (SHORT 단계에 연결된 StudySession 사용)
      // 실기는 MINI와 SHORT가 같은 StudySession을 공유하므로 MINI 세션 사용
      // SHORT 5문제만 사용 (LONG 제거)
      List<Question> combined = questionRepository.pickRandomByTopic(
          topicId, ExamMode.PRACTICAL, QuestionType.SHORT, PageRequest.of(0, PRACTICAL_SIZE));

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
    String next = completed ? "SHORT_SUMMARY" : null;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "SHORT_SET",
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

    // 2. MINI 단계에서 StudySession 가져오기 (실기는 MINI와 SHORT가 같은 StudySession 공유)
    LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
    StudySession session = miniStep.getStudySession();

    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    // 3. 세션에 할당된 문제인지 검증
    List<StudySessionItem> sessionItems = sessionManager.items(session.getId());
    Set<Long> allocatedQuestionIds = sessionItems.stream()
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // SHORT 단계 문제만
        .map(StudySessionItem::getQuestionId)
        .collect(Collectors.toSet());

    for (PracticalDtos.PracticalAnswer answer : req.answers()) {
      if (!allocatedQuestionIds.contains(answer.questionId())) {
        throw new IllegalStateException("세션에 할당되지 않은 문제입니다: " + answer.questionId());
      }
    }

    // 순서는 세션에 할당된 순서 사용
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // SHORT 단계 문제만
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
      Map<String, Object> aiExplainMap = new HashMap<>();
      aiExplainMap.put("explain", result.explain());
      aiExplainMap.put("tips", result.tips());
      aiExplainMap.put("aiFailed", result.aiFailed());
      StudySessionItem item = sessionManager.upsertItem(
          session,
          question.getId(),
          orderNo,
          toJson(answerJson),
          correct,
          correct ? 100 : 0,  // 하위 호환성을 위해 점수도 저장 (맞으면 100, 틀리면 0)
          toJson(aiExplainMap)
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
        .filter(item -> item.getOrderNo() > MINI_SIZE)  // SHORT 단계 문제만
        .toList();
    int allocatedTotal = allPracticalItems.size();

    // 모든 문제를 풀었는지 확인 (답변이 있는 문제 수)
    long answeredCount = allPracticalItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allPracticalQuestionsAnswered = allocatedTotal > 0 && answeredCount >= allocatedTotal;

    // 4. LearningStep 업데이트
    // 하위 호환성: "SHORT" 단계가 없으면 "PRACTICAL" 단계를 찾아봄 (이전 버전의 세션)
    LearningStep practicalStep;
    try {
      practicalStep = learningSessionService.getStep(learningSession, "SHORT");
    } catch (NoSuchElementException e) {
      // 이전 버전의 세션일 수 있으므로 "PRACTICAL" 단계 시도
      try {
        practicalStep = learningSessionService.getStep(learningSession, "PRACTICAL");
      } catch (NoSuchElementException e2) {
        throw new IllegalStateException("SHORT 또는 PRACTICAL 단계가 없습니다. 세션을 다시 시작해주세요.", e2);
      }
    }
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
    boolean finalCompleted = everCompleted || allPracticalQuestionsAnswered;  // 모든 문제를 풀었으면 완료

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

    // 진정한 완료 설정 (SHORT 단계 완료 시 모든 문제를 맞췄을 때)
    boolean newlyCompleted = false;
    if (finalCompleted && allWrongIds.isEmpty() && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
      newlyCompleted = true;
    }

    // StudySession 종료 처리 (MICRO 모드 PRACTICAL 완료 시)
    // MICRO 실기는 MINI(4문제) + PRACTICAL(5문제) = 총 9문제 모두 맞아야 완료
    boolean practicalAllCorrect = finalCompleted && allWrongIds.isEmpty();
    String sessionMode = learningSession.getMode();

    // MINI 단계도 모두 맞았는지 확인
    boolean miniAllPassed = false;
    if ("MICRO".equals(sessionMode)) {
      if (miniStep != null) {
        Map<String, Object> miniMeta = parseJson(miniStep.getMetadataJson());
        int miniTotal = readInt(miniMeta, "total");
        int miniCorrect = readInt(miniMeta, "correct");
        boolean miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
        miniAllPassed = miniPassed && miniTotal >= MINI_SIZE && miniCorrect == miniTotal;
      }
    }

    // MICRO 실기: MINI와 PRACTICAL 모두 완료되고 모두 맞았을 때만 세션 종료
    boolean microAllCorrect = practicalAllCorrect && ("REVIEW".equals(sessionMode) || miniAllPassed);
    
    // 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    List<StudySessionItem> allItems = sessionManager.items(session.getId());
    long allSessionAnsweredCount = allItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allSessionQuestionsAnswered = allSessionAnsweredCount == allItems.size() && allItems.size() >= (MINI_SIZE + PRACTICAL_SIZE);
    
    // 세션이 완료되었을 때 finalizeStudySession 호출
    // 모든 문제가 제출되었으면 finalizeStudySession 호출 (이미 완료된 경우도 재계산)
    if (allSessionQuestionsAnswered && session != null) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      
      // MICRO 모드: MINI도 모두 맞았는지 확인 (passed 판정용)
      boolean passedWithMini = result.passed();
      if ("MICRO".equals(sessionMode)) {
        // MINI도 모두 맞아야 passed = true
        passedWithMini = result.passed() && miniAllPassed;
        
        // passed 값 업데이트 (MINI 완료 여부 반영)
        if (passedWithMini != result.passed()) {
          sessionManager.updatePassed(session, passedWithMini);
        }
      }
      
      // LearningStep 메타데이터 업데이트
      practicalMeta.put("total", result.total());
      practicalMeta.put("correct", result.correct());
      practicalMeta.put("scorePct", (int) Math.round(result.scorePct()));
      practicalMeta.put("passed", passedWithMini);
      practicalMeta.put("completed", true);
      practicalMeta.put("wrongQuestionIds", allWrongIds);
      practicalStep.setMetadataJson(toJson(practicalMeta));
      practicalStep.setScorePct((int) Math.round(result.scorePct()));
      practicalStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(practicalStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      sessionManager.saveStepMeta(session, "practical", practicalMeta);
      practicalStep.setMetadataJson(metadataJson);
      practicalStep.setScorePct(scorePct);
      practicalStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(practicalStep);
    }

    // [B] MICRO 완료 체크 및 XP 지급
    if (allSessionQuestionsAnswered && "MICRO".equals(sessionMode)) {
      checkMicroCompletionAndXp(learningSession, session, ExamMode.PRACTICAL);
    }

    // 다음 단계 결정: 오답이 있으면 REVIEW_WRONG, 없으면 SUMMARY
    String nextStep = finalCompleted
        ? (!allWrongIds.isEmpty() ? "SHORT_REVIEW_WRONG" : "SHORT_SUMMARY")
        : "SHORT_SET";

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "PRACTICAL",
        "SHORT_SET",
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
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalSet> practicalReviewSet(Long rootTopicId, Long learningSessionId) {
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

    // 2. SHORT 단계 조회
    LearningStep practicalStep = learningSessionService.getStep(learningSession, "SHORT");
    StudySession studySession = practicalStep.getStudySession();

    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다. 세션을 먼저 시작해주세요.");
    }

    // 3. 세션에 할당된 문제 조회 (랜덤이 아님!)
    List<StudySessionItem> items = sessionManager.items(studySession.getId());
    List<Long> questionIds = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(StudySessionItem::getQuestionId)
        .toList();

    if (questionIds.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // 4. 문제 상세 정보 조회
    Map<Long, Question> questionMap = questionRepository.findByIdIn(questionIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환
    List<PracticalDtos.PracticalQuestion> practicalQuestions = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          return new PracticalDtos.PracticalQuestion(
              q.getId(),
              q.getType().name(),
              Optional.ofNullable(q.getStem()).orElse(""),
              q.getImageUrl());
        })
        .toList();

    // 6. 단계 상태 확인
    String status = practicalStep.getStatus();
    boolean completed = "COMPLETE".equals(status);

    // 상태 변경은 advance API를 통해 수행되어야 함
    // 단계가 READY 상태이면 IN_PROGRESS로 표시만 함 (실제 변경은 advance에서)
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "REVIEW",
        "SHORT_REVIEW_SET",
        completed ? "COMPLETE" : "IN_PROGRESS",
        completed ? "REVIEW_WRONG" : null,
        sessionManager.loadMeta(studySession),
        new PracticalDtos.PracticalSet(practicalQuestions),
        learningSession.getId()
    );
  }

  @Transactional
  public FlowDtos.StepEnvelope<PracticalDtos.PracticalReviewSubmitResp> practicalReviewSubmit(
      PracticalDtos.PracticalReviewSubmitReq req) {

    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(req.learningSessionId());
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.rootTopicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    if (!"REVIEW".equals(learningSession.getMode())) {
      throw new IllegalStateException("Review 모드가 아닙니다.");
    }

    LearningStep practicalStep = learningSessionService.getStep(learningSession, "SHORT");

    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = practicalStep.getStudySession();
    if (session == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

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
          "SHORT_REVIEW"
      );
      pushProgressHook(userId, question.getType(), correct, correct ? 100 : 0, question.getId());
      // 실기 리뷰도 Progress 에 반영
      updateProgress(userId, question.getTopicId(), correct);
    }

    boolean allCorrect = !items.isEmpty() && wrongIds.isEmpty();
    // 첫 번째 scorePct 로컬 변수 제거 (중복 이름 방지 + 실제 사용 X)

    int prevTotal;
    int prevCorrect;
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds;

    // 3. LearningStep (PRACTICAL) 업데이트 (이전 메타데이터 불러와서 누적)
    Map<String, Object> prevPracticalMeta = parseJson(practicalStep.getMetadataJson());
    Map<String, Object> practicalMeta = new HashMap<>(prevPracticalMeta);

    // 누적 로직
    prevTotal = readInt(prevPracticalMeta, "total");
    prevCorrect = readInt(prevPracticalMeta, "correct");
    prevWrongIds = prevPracticalMeta.get("wrongQuestionIds") instanceof List<?>
        ? (List<Long>) prevPracticalMeta.get("wrongQuestionIds")
        : new ArrayList<>();

    int newTotal = prevTotal + req.answers().size();
    int newCorrect = prevCorrect + correctCount;
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    allWrongIds.addAll(wrongIds);
    boolean prevCompleted = Boolean.TRUE.equals(prevPracticalMeta.get("completed"));
    boolean practicalCompleted = allCorrect;  // 모든 문제를 맞춰야 완료
    boolean finalCompleted = prevCompleted || practicalCompleted;
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;

    practicalMeta.put("total", newTotal);
    practicalMeta.put("correct", newCorrect);
    practicalMeta.put("completed", finalCompleted);
    practicalMeta.put("scorePct", accumulatedScorePct);
    practicalMeta.put("wrongQuestionIds", allWrongIds);
    practicalMeta.put("lastSubmittedAt", Instant.now().toString());

    String metadataJson = toJson(practicalMeta);

    // 4. 진정한 완료 설정 (PRACTICAL 완료 시 - 모든 문제를 맞춰야 완료)
    boolean newlyCompleted = false;
    if (finalCompleted && allWrongIds.isEmpty() && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
      newlyCompleted = true;
    }

    // 5-1. 세션이 완료되었는지 확인 (모든 문제를 제출했는지)
    List<StudySessionItem> allReviewPracticalItems = sessionManager.items(session.getId());
    long reviewPracticalAnsweredCount = allReviewPracticalItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();
    boolean allReviewPracticalQuestionsAnswered = reviewPracticalAnsweredCount >= REVIEW_SIZE && 
        reviewPracticalAnsweredCount == allReviewPracticalItems.size();
    
    // 5-2. 세션이 완료되었을 때 finalizeStudySession 호출
    // 모든 문제가 제출되었으면 finalizeStudySession 호출 (이미 완료된 경우도 재계산)
    if (allReviewPracticalQuestionsAnswered && session != null) {
      // finalizeStudySession: study_session_item 기준으로 정확히 계산
      StudySessionManager.FinalizeResult result = sessionManager.finalizeStudySession(session);
      
      // LearningStep 메타데이터 업데이트
      practicalMeta.put("total", result.total());
      practicalMeta.put("correct", result.correct());
      practicalMeta.put("scorePct", (int) Math.round(result.scorePct()));
      practicalMeta.put("passed", result.passed());
      practicalMeta.put("completed", true);
      practicalMeta.put("wrongQuestionIds", allWrongIds);
      practicalStep.setMetadataJson(toJson(practicalMeta));
      practicalStep.setScorePct((int) Math.round(result.scorePct()));
      practicalStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(practicalStep);
    } else {
      // 아직 완료되지 않은 경우 기존 로직 유지
      sessionManager.saveStepMeta(session, "practical", practicalMeta);
      practicalStep.setMetadataJson(metadataJson);
      practicalStep.setScorePct(accumulatedScorePct);
      practicalStep.setUpdatedAt(Instant.now());
      learningStepRepository.save(practicalStep);
    }

    // 6. [C] REVIEW 완료 체크 및 XP 지급
    if (allReviewPracticalQuestionsAnswered && "REVIEW".equals(learningSession.getMode())) {
      checkReviewCompletionAndXp(learningSession, session, ExamMode.PRACTICAL);
    }

    // 상태는 메타데이터 기반으로 판단 (실제 상태 변경은 advance에서)
    String status = newTotal >= REVIEW_SIZE ? "COMPLETE" : "IN_PROGRESS";
    String nextStep = newTotal >= REVIEW_SIZE ? "REVIEW_WRONG" : null;

    return new FlowDtos.StepEnvelope<>(
        session.getId(),
        "REVIEW",
        "SHORT_REVIEW_SET",
        status,
        nextStep,
        sessionManager.loadMeta(session),
        new PracticalDtos.PracticalReviewSubmitResp(
            newTotal,
            newCorrect,
            items,
            allWrongIds
        ),
        learningSession.getId()
    );
  }

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> practicalReviewSummary(Long rootTopicId, Long learningSessionId) {
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

    // 2. StudySession 조회 (실기 리뷰는 SHORT 단계를 사용)
    LearningStep practicalStep = learningSessionService.getStep(learningSession, "SHORT");
    StudySession session = practicalStep.getStudySession();

    // 3. 약점 태그 계산 및 summary_json 로드
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
          .collect(Collectors.toMap(Question::getId, q -> q));
      List<UserAnswer> answers = sessionAnswers.stream()
          .filter(ans -> questionCache.containsKey(ans.getQuestionId()))
          .toList();
      mistakes = collectMistakes(answers, questionCache);
    }

    // 4. LearningStep과 summary_json에서 메타데이터 추출 (summary_json 우선)
    Map<String, Object> practicalMeta = parseJson(practicalStep.getMetadataJson());
    
    // summary_json에서 practical 정보 가져오기 (우선순위 높음)
    if (session != null && !meta.isEmpty()) {
      Object practicalRaw = meta.get("practical");
      if (practicalRaw instanceof Map<?, ?> practicalFromSummary) {
        Map<String, Object> practicalFromSummaryMap = new HashMap<>();
        practicalFromSummary.forEach((k, v) -> practicalFromSummaryMap.put(String.valueOf(k), v));
        // summary_json의 정보로 덮어쓰기
        practicalMeta.putAll(practicalFromSummaryMap);
      }
    }

    int practicalTotal = readInt(practicalMeta, "total");
    int practicalCorrect = readInt(practicalMeta, "correct");
    boolean practicalCompleted = Boolean.TRUE.equals(practicalMeta.get("completed"));

    boolean completed = practicalCompleted;

    String topicTitle = "";
    try {
      var curriculum = curriculumGateway.getConceptWithTopic(rootTopicId);
      topicTitle = curriculum.topicTitle();
    } catch (Exception ignored) {
      // 커리큘럼 장애 시에도 요약은 진행
    }

    String summaryText = aiExplanationService.summarizePractical(
        topicTitle,
        practicalTotal,
        practicalCorrect,
        mistakes
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
        // 정답률 계산 (summary_json에서 가져온 practicalMeta 우선 사용)
        double scorePct;
        
        // 1순위: practicalMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 가장 정확)
        Object scorePctObj = practicalMeta.get("scorePct");
        if (scorePctObj != null && scorePctObj instanceof Number) {
          scorePct = ((Number) scorePctObj).doubleValue();
          System.out.println("[PracticalService.practicalReviewSummary] scorePct from practicalMeta: " + scorePct);
        } else if (practicalTotal > 0) {
          // 2순위: 계산하기
          scorePct = (practicalCorrect * 100.0) / practicalTotal;
          System.out.println("[PracticalService.practicalReviewSummary] scorePct calculated: " + scorePct + 
                            " (practicalTotal=" + practicalTotal + ", practicalCorrect=" + practicalCorrect + ")");
        } else if (session.getScorePct() != null) {
          // 3순위: session에서 가져오기
          scorePct = session.getScorePct();
          System.out.println("[PracticalService.practicalReviewSummary] scorePct from session: " + scorePct);
        } else {
          // 4순위: 기본값 0.0
          scorePct = 0.0;
          System.out.println("[PracticalService.practicalReviewSummary] scorePct default: 0.0");
        }
        
        // scorePct는 반드시 0.0 이상 100.0 이하의 유효한 값이어야 함
        if (scorePct < 0.0 || scorePct > 100.0 || Double.isNaN(scorePct)) {
          System.err.println("[PracticalService.practicalReviewSummary] Invalid scorePct: " + scorePct + ", using 0.0");
          scorePct = 0.0;
        }
        
        // 로깅: 실제 전달되는 scorePct 값 확인
        System.out.println("[PracticalService.practicalReviewSummary] XP 지급 요청: sessionId=" + session.getId() + 
                          ", practicalTotal=" + practicalTotal + ", practicalCorrect=" + practicalCorrect + 
                          ", scorePct=" + scorePct);
        
        // 1. XP 지급 요청 (정답률 기반) - scorePct는 절대 null이 아님
        ProgressXpClient.XpEarnRequest xpRequest = new ProgressXpClient.XpEarnRequest(
            "SHORT_REVIEW",
            session.getId(),
            rootTopicId,
            scorePct  // 항상 유효한 Double 값 (null 아님)
        );
        
        System.out.println("[PracticalService.practicalReviewSummary] XP 요청 상세: activityType=" + xpRequest.activityType() + 
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
                ExamMode.PRACTICAL.name(),
                "REVIEW",
                rootTopicId
            ));
          } catch (Exception hookEx) {
            // hook 실패는 학습 흐름을 막지 않음
            System.err.println("Failed to call flow-complete hook: " + hookEx.getMessage());
          }
          
        } catch (Exception e) {
          // XP 지급 실패는 학습 흐름을 막지 않음, 로깅만 수행
          System.err.println("Failed to grant XP in practicalReviewSummary (REVIEW): " + e.getMessage());
          e.printStackTrace();
        }
    }
    
    // REVIEW 세션의 score_pct와 passed 업데이트
    // summary_json에 완료 정보가 있으면 그것을 사용해서 직접 업데이트
    // 세션이 완료되었거나 (finished_at이 있거나, 모든 문제를 풀었거나) score_pct가 0이면 업데이트
    if (session != null && practicalTotal > 0) {
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
      
      // practicalMeta에서 정보 가져오기
      double scorePctFromMeta = practicalTotal > 0 ? (practicalCorrect * 100.0) / practicalTotal : 0.0;
      boolean passedFromMeta = practicalCorrect == practicalTotal && practicalTotal > 0;
      
      // scorePctFromMeta를 practicalMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 더 정확)
      Object scorePctObj = practicalMeta.get("scorePct");
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
          System.out.println("[PracticalService.practicalReviewSummary] Updating session: sessionId=" + session.getId() + 
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
        0,              // Review 모드에는 MINI 없음
        0,
        false,
        practicalTotal,
        practicalCorrect,
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
        status,
        null,
        meta,
        payload,
        learningSession.getId()
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

    // 모드에 따라 적절한 단계 선택
    String practicalStepName;
    String miniStepName;

    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(learningSession.getMode())) {
      // difficulty 기반 보조학습: MINI 단계가 없고 ASSIST_PRACTICAL_DIFFICULTY 단계만 있음
      practicalStepName = "ASSIST_PRACTICAL_DIFFICULTY";
      miniStepName = null;
    } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(learningSession.getMode())) {
      // weakness 기반 보조학습: MINI 단계가 없고 ASSIST_PRACTICAL_WEAKNESS 단계만 있음
      practicalStepName = "ASSIST_PRACTICAL_WEAKNESS";
      miniStepName = null;
    } else if ("ASSIST_PRACTICAL_CATEGORY".equals(learningSession.getMode())) {
      // category 기반 보조학습: MINI 단계가 없고 ASSIST_PRACTICAL_CATEGORY 단계만 있음
      practicalStepName = "ASSIST_PRACTICAL_CATEGORY";
      miniStepName = null;
    } else {
      // 일반 학습: MINI와 SHORT 단계 사용
      practicalStepName = "SHORT";
      miniStepName = "MINI";
    }

    // StudySession 가져오기
    StudySession session;
    Map<String, Object> miniMeta;

    int miniTotal = 0;
    int miniCorrect = 0;
    boolean miniPassed = true; // difficulty 기반 보조학습은 MINI가 없으므로 항상 통과로 간주

    if (miniStepName != null) {
      LearningStep miniStep = learningSessionService.getStep(learningSession, miniStepName);
      session = miniStep.getStudySession();
      miniMeta = parseJson(miniStep.getMetadataJson());
      miniTotal = readInt(miniMeta, "total");
      miniCorrect = readInt(miniMeta, "correct");
      miniPassed = Boolean.TRUE.equals(miniMeta.get("passed"));
    } else {
      LearningStep practicalStep = learningSessionService.getStep(learningSession, practicalStepName);
      session = practicalStep.getStudySession();
      miniMeta = Map.of();
    }

    // LearningStep에서 메타데이터 추출
    LearningStep practicalStep = learningSessionService.getStep(learningSession, practicalStepName);
    Map<String, Object> practicalMeta = parseJson(practicalStep.getMetadataJson());

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
          .filter(q -> topicId == 0L || Objects.equals(q.getTopicId(), topicId)) // difficulty 기반 보조학습은 topicId=0
          .collect(Collectors.toMap(Question::getId, q -> q));
      List<UserAnswer> topicAnswers = sessionAnswers.stream()
          .filter(ans -> questionCache.containsKey(ans.getQuestionId()))
          .toList();
      mistakes = collectMistakes(topicAnswers, questionCache);
    }

    boolean completed = miniPassed && practicalCompleted;
    int totalSolved = miniTotal + practicalTotal;
    int practicalPassedCount = practicalTotal > 0 ? Math.max(0, practicalTotal - readList(practicalMeta, "wrongQuestionIds").size()) : 0;

    // 토픽 제목도 cert-service(커리큘럼)에서 가져오도록 수정
    String topicTitle = "";
    if (topicId != 0L) { // difficulty 기반 보조학습(topicId=0)은 토픽 제목 조회 생략
      try {
        var curriculum = curriculumGateway.getConceptWithTopic(topicId);
        topicTitle = curriculum.topicTitle();
      } catch (Exception ignored) {
        // 커리큘럼 장애 시에도 요약은 진행
      }
    } else {
      if ("ASSIST_PRACTICAL_CATEGORY".equals(learningSession.getMode())) {
        topicTitle = "카테고리 기반 보조학습";
      } else {
        topicTitle = "난이도 기반 보조학습";
      }
    }

    // 실기 요약: 실기 문제(Practical)만 집중 (MINI는 OX 문제이므로 실기 요약에서 제외)
    // totalSolved가 아니라 practicalTotal과 practicalCorrect만 전달
    String summaryText = aiExplanationService.summarizePractical(
        topicTitle,
        practicalTotal,  // 실기 문제 총 수만 전달 (MINI 제외)
        practicalCorrect,  // 실기 문제 정답 수만 전달
        mistakes
    );

    // XP 정보 초기값
    Integer earnedXp = null;
    Long totalXp = null;
    Integer level = null;
    Integer xpToNextLevel = null;
    Boolean leveledUp = null;
    Integer levelUpRewardPoints = null;

    String status = completed ? "COMPLETE" : "IN_PROGRESS";

    // MICRO 모드: MINI와 PRACTICAL 모두 완료되었는지 확인
    boolean trulyCompleted = Boolean.TRUE.equals(learningSession.getTrulyCompleted());
    boolean miniAllPassed = false;
    
    if ("MICRO".equals(learningSession.getMode())) {
      // MINI가 모두 맞았는지 확인
      miniAllPassed = miniPassed && miniTotal >= MINI_SIZE && miniCorrect == miniTotal;
      // PRACTICAL도 모두 맞았는지 확인
      boolean practicalAllCorrect = completed && practicalTotal >= PRACTICAL_SIZE && practicalCorrect == practicalTotal;
      
      // XP 지급 (xp_granted=0이면 항상 지급, 정답률 기반)
      // 조건: xp_granted=0 (passed와 상관없이 정답률에 따라 XP 지급)
      // MICRO는 MINI와 PRACTICAL 모두 완료되었을 때 XP 지급
      if (sessionId != null && session != null && !Boolean.TRUE.equals(session.getXpGranted())) {
        try {
          // 정답률 계산 (MICRO는 MINI+PRACTICAL 합산 정답률 사용)
          // 1순위: practicalMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 가장 정확)
          double scorePct;
          Object scorePctObj = practicalMeta.get("scorePct");
          if (scorePctObj != null && scorePctObj instanceof Number) {
            double practicalScorePct = ((Number) scorePctObj).doubleValue();
            // MICRO는 MINI + PRACTICAL 합산 정답률 사용
            if (totalSolved > 0) {
              int totalCorrect = miniCorrect + practicalCorrect;
              scorePct = (totalCorrect * 100.0) / totalSolved;
            } else {
              scorePct = practicalScorePct; // PRACTICAL만 있는 경우
            }
            System.out.println("[PracticalService.summary] scorePct from practicalMeta: " + practicalScorePct + 
                             ", calculated total: " + scorePct + " (totalSolved=" + totalSolved + ", totalCorrect=" + (miniCorrect + practicalCorrect) + ")");
          } else if (totalSolved > 0) {
            // 2순위: 계산하기
            int totalCorrect = miniCorrect + practicalCorrect;
            scorePct = (totalCorrect * 100.0) / totalSolved;
            System.out.println("[PracticalService.summary] scorePct calculated: " + scorePct + 
                            " (totalSolved=" + totalSolved + ", totalCorrect=" + totalCorrect + ")");
          } else if (session.getScorePct() != null && session.getScorePct() > 0.0) {
            // 3순위: session에서 가져오기 (0이 아닐 때만)
            scorePct = session.getScorePct();
            System.out.println("[PracticalService.summary] scorePct from session: " + scorePct);
          } else {
            // 4순위: 기본값 0.0
            scorePct = 0.0;
            System.out.println("[PracticalService.summary] scorePct default: 0.0");
          }
          
          // scorePct는 반드시 0.0 이상 100.0 이하의 유효한 값이어야 함
          if (scorePct < 0.0 || scorePct > 100.0 || Double.isNaN(scorePct)) {
            System.err.println("[PracticalService.summary] Invalid scorePct: " + scorePct + ", using 0.0");
            scorePct = 0.0;
          }
          
          System.out.println("[PracticalService.summary] XP 지급 요청 (MICRO): sessionId=" + session.getId() + 
                            ", totalSolved=" + totalSolved + ", totalCorrect=" + (miniCorrect + practicalCorrect) + 
                            ", scorePct=" + scorePct);
          
          // 1. XP 지급 요청 (정답률 기반) - scorePct는 절대 null이 아님
          ProgressXpClient.XpEarnRequest xpRequest = new ProgressXpClient.XpEarnRequest(
              "PRACTICAL_MICRO",
              session.getId(),
              topicId,
              scorePct  // 항상 유효한 Double 값 (null 아님)
          );
          
          System.out.println("[PracticalService.summary] XP 요청 상세: activityType=" + xpRequest.activityType() + 
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
            // MINI 세션도 표시 (MICRO는 MINI+PRACTICAL 합쳐서 하나의 XP)
            LearningStep miniStep = learningSessionService.getStep(learningSession, "MINI");
            StudySession miniSession = miniStep != null ? miniStep.getStudySession() : null;
            if (miniSession != null && !Boolean.TRUE.equals(miniSession.getXpGranted())) {
              sessionManager.markXpGranted(miniSession);
            }
            
            // 4. 기존 hook도 호출 (다른 통계 처리용)
            try {
              progressHookClient.flowComplete(new ProgressHookClient.FlowCompletePayload(
                  userId,
                  ExamMode.PRACTICAL.name(),
                  "MICRO",
                  topicId
              ));
            } catch (Exception hookEx) {
              // hook 실패는 학습 흐름을 막지 않음
              System.err.println("Failed to call flow-complete hook: " + hookEx.getMessage());
            }
            
        } catch (Exception e) {
          // XP 지급 실패는 학습 흐름을 막지 않음, 로깅만 수행
          System.err.println("Failed to grant XP in practicalSummary (MICRO): " + e.getMessage());
          e.printStackTrace();
        }
      }
    }
    
    // PRACTICAL 세션의 score_pct와 passed 업데이트 (모든 모드에 대해)
    // summary_json에 완료 정보가 있으면 그것을 사용해서 직접 업데이트
    if (session != null && practicalTotal > 0) {
      // practicalMeta에서 정보 가져오기
      double scorePctFromMeta = practicalTotal > 0 ? (practicalCorrect * 100.0) / practicalTotal : 0.0;
      boolean passedFromMeta = practicalCorrect == practicalTotal && practicalTotal > 0;
      
      // scorePctFromMeta를 practicalMeta에서 직접 가져오기 (summary_json에서 파싱한 값이 더 정확)
      Object scorePctObj = practicalMeta.get("scorePct");
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
        boolean sessionFinished = session.getFinishedAt() != null;
        boolean sessionCompleted = Boolean.TRUE.equals(session.getCompleted());
        boolean allQuestionsAnswered = practicalCompleted || (practicalTotal >= PRACTICAL_SIZE && practicalCorrect + (practicalTotal - practicalCorrect) == practicalTotal);
        
        if (sessionFinished || sessionCompleted || practicalCompleted || allQuestionsAnswered) {
          System.out.println("[PracticalService.summary] Updating PRACTICAL session: sessionId=" + session.getId() + 
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
        miniTotal,
        miniCorrect,
        miniPassed,
        practicalTotal,
        practicalCorrect,
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
        "PRACTICAL",
        "SHORT_SUMMARY",
        status,
        null,
        meta,
        payload,
        learningSession.getId()
    );
  }

  public PracticalDtos.PracticalGradeOneResp gradeOnePractical(Long learningSessionId, PracticalDtos.PracticalGradeOneReq req) {
    // 문제 조회하여 answerKey 가져오기
    Question question = questionRepository.findById(req.questionId())
        .orElseThrow(() -> new NoSuchElementException("문제를 찾을 수 없습니다: " + req.questionId()));

    // AI 해설 생성 및 실패 여부 확인
    AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
        question, req.userText());

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
        Optional.ofNullable(question.getAnswerKey()).orElse(""),
        item.baseExplanation(),
        item.aiExplanation(),
        result.aiFailed()
    );
  }

  @Transactional
  public PracticalDtos.PracticalReviewGradeOneResp gradeOnePracticalReview(Long learningSessionId, PracticalDtos.PracticalReviewGradeOneReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!learningSession.getTopicId().equals(req.rootTopicId())) {
      throw new IllegalStateException("토픽이 일치하지 않습니다.");
    }
    if (!"REVIEW".equals(learningSession.getMode())) {
      throw new IllegalStateException("Review 모드가 아닙니다.");
    }

    LearningStep practicalStep = learningSessionService.getStep(learningSession, "SHORT");

    // 2. StudySession 조회 (이미 할당되어 있어야 함)
    StudySession session = practicalStep.getStudySession();
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

    // 4. 문제 조회 및 AI 채점
    Question question = questionRepository.findById(req.questionId())
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .orElseThrow(() -> new NoSuchElementException("Question not found: " + req.questionId()));

    AIExplanationService.PracticalResult result = aiExplanationService.explainAndScorePractical(
        question, req.userText());
    boolean isCorrect = Optional.ofNullable(result.correct()).orElse(false);
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

    // 5. 세션에 아이템 저장 (순서는 세션에 할당된 순서 사용)
    Map<Long, Integer> questionOrderMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, StudySessionItem::getOrderNo));
    int orderNo = questionOrderMap.get(question.getId());

    Map<String, Object> answerJson = new HashMap<>();
    answerJson.put("answer", Optional.ofNullable(req.userText()).orElse(""));
    answerJson.put("correct", isCorrect);
    answerJson.put("tips", result.tips());

    StudySessionItem item = sessionManager.upsertItem(
        session,
        question.getId(),
        orderNo,
        toJson(answerJson),
        isCorrect,
        isCorrect ? 100 : 0,
        toJson(Map.of("explain", result.explain(), "tips", result.tips()))
    );

    persistUserAnswer(userId, question, req.userText(), isCorrect, isCorrect ? 100 : 0, session, item, "PRACTICAL_REVIEW");
    pushProgressHook(userId, question.getType(), isCorrect, isCorrect ? 100 : 0, question.getId());
    updateProgress(userId, question.getTopicId(), isCorrect);

    // 6. LearningStep 메타데이터 업데이트 (누적)
    Map<String, Object> prevPracticalMeta = parseJson(practicalStep.getMetadataJson());
    Map<String, Object> practicalMeta = new HashMap<>(prevPracticalMeta);

    int prevTotal = readInt(prevPracticalMeta, "total");
    int prevCorrect = readInt(prevPracticalMeta, "correct");
    @SuppressWarnings("unchecked")
    List<Long> prevWrongIds = prevPracticalMeta.get("wrongQuestionIds") instanceof List<?>
        ? (List<Long>) prevPracticalMeta.get("wrongQuestionIds")
        : new ArrayList<>();

    int newTotal = prevTotal + 1;
    int newCorrect = prevCorrect + (isCorrect ? 1 : 0);
    List<Long> allWrongIds = new ArrayList<>(prevWrongIds);
    if (!isCorrect) {
      allWrongIds.add(question.getId());
    }
    boolean allCorrect = newCorrect == newTotal;
    boolean prevCompleted = Boolean.TRUE.equals(prevPracticalMeta.get("completed"));
    boolean finalCompleted = prevCompleted || allCorrect;
    int accumulatedScorePct = newTotal > 0 ? (newCorrect * 100) / newTotal : 0;

    practicalMeta.put("total", newTotal);
    practicalMeta.put("correct", newCorrect);
    practicalMeta.put("completed", finalCompleted);
    practicalMeta.put("scorePct", accumulatedScorePct);
    practicalMeta.put("wrongQuestionIds", allWrongIds);
    practicalMeta.put("lastSubmittedAt", Instant.now().toString());

    String metadataJson = toJson(practicalMeta);

    // 7. 진정한 완료 설정 (PRACTICAL 완료 시)
    if (finalCompleted && learningSession.getTrulyCompleted() == null) {
      learningSession.setTrulyCompleted(true);
      learningSessionService.saveLearningSession(learningSession);
    }

    // 8. StudySession의 summaryJson에도 저장 (하위 호환성)
    sessionManager.saveStepMeta(session, "practical", practicalMeta);

    // 9. 메타데이터만 업데이트 (상태 변경은 advance API를 통해 수행)
    // SHORT 단계의 메타데이터를 LearningStep에 저장 (advance 호출 시 사용)
    practicalStep.setMetadataJson(metadataJson);
    practicalStep.setScorePct(accumulatedScorePct);
    practicalStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(practicalStep);

    // 실기 Review 모드는 AI 해설을 제공함 (일반 실기 모드와 동일)
    String aiExplanation = Optional.ofNullable(result.explain()).orElse("");
    Boolean aiExplanationFailed = Optional.ofNullable(result.aiFailed()).orElse(false);

    return new PracticalDtos.PracticalReviewGradeOneResp(
        isCorrect,
        Optional.ofNullable(question.getAnswerKey()).orElse(""),
        explanation,
        aiExplanation,
        aiExplanationFailed
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

    // stepCode(MICRO_OX / SHORT_SET / REVIEW ...) → UserAnswer.source 로 매핑
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
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }

    // 모드에 따라 적절한 단계/소스를 선택
    String stepCode;
    String stepName;
    StudySession session;

    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(learningSession.getMode())) {
      // difficulty 기반 보조학습
      stepName = "ASSIST_PRACTICAL_DIFFICULTY";
      stepCode = "ASSIST_PRACTICAL_DIFFICULTY";
      LearningStep difficultyStep = learningSessionService.getStep(learningSession, stepName);
      session = difficultyStep.getStudySession();
    } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(learningSession.getMode())) {
      // weakness 기반 보조학습
      stepName = "ASSIST_PRACTICAL_WEAKNESS";
      stepCode = "ASSIST_PRACTICAL_WEAKNESS";
      LearningStep weaknessStep = learningSessionService.getStep(learningSession, stepName);
      session = weaknessStep.getStudySession();
    } else if ("ASSIST_PRACTICAL_CATEGORY".equals(learningSession.getMode())) {
      // category 기반 보조학습
      stepName = "ASSIST_PRACTICAL_CATEGORY";
      stepCode = "ASSIST_PRACTICAL_CATEGORY";
      LearningStep categoryStep = learningSessionService.getStep(learningSession, stepName);
      session = categoryStep.getStudySession();
    } else if ("REVIEW".equals(learningSession.getMode())) {
      // 실기 REVIEW 모드: SHORT 단계의 StudySession 사용
      stepName = "SHORT";
      stepCode = "SHORT_REVIEW";
      LearningStep practicalStep = learningSessionService.getStep(learningSession, stepName);
      session = practicalStep.getStudySession();
    } else {
      // Micro 실기 모드: MINI 단계와 SHORT 단계가 같은 StudySession을 공유하므로 MINI 단계에서 조회
      stepName = "MINI";
      stepCode = "SHORT_SET";
      LearningStep miniStep = learningSessionService.getStep(learningSession, stepName);
      session = miniStep.getStudySession();
    }

    if (session == null) {
      return new WrongRecapDtos.WrongRecapSet(List.of());
    }

    // 기존 wrongRecapBySession 로직 재사용
    return wrongRecapBySession(session.getId(), stepCode);
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
    // 실기는 SHORT만 사용 (LONG 제거)
    return question.getType() == QuestionType.SHORT;
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
    Boolean aiExplanationFailed = null;
    if (sessionItem != null && sessionItem.getAiExplainJson() != null) {
      try {
        Map<String, Object> aiExplainMap = parseJson(sessionItem.getAiExplainJson());
        Object explainObj = aiExplainMap.get("explain");
        if (explainObj != null) {
          aiExplanation = explainObj.toString();
        }
        Object aiFailedObj = aiExplainMap.get("aiFailed");
        if (aiFailedObj instanceof Boolean) {
          aiExplanationFailed = (Boolean) aiFailedObj;
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
        aiExplanation,
        aiExplanationFailed
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
      case "MICRO_OX", "PRACTICAL_MINI" -> "PRACTICAL_MINI";              // 실기 OX (MINI 단계)
      case "SHORT_SET", "MICRO_PRACTICAL" -> "MICRO_PRACTICAL";       // 실기 Micro 세트 (SHORT 단계)
      case "REVIEW", "SHORT_REVIEW_SET", "SHORT_REVIEW" -> "SHORT_REVIEW"; // 실기 Review (SHORT 단계)
      case "ASSIST_PRACTICAL_DIFFICULTY", "ASSIST_PRACTICAL_WEAKNESS", "ASSIST_PRACTICAL_CATEGORY" -> "ASSIST_PRACTICAL"; // 보조학습 (difficulty/weakness/category)
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

  /**
   * [C] REVIEW 완료 체크 및 XP 지급 (실기)
   * REVIEW 세션이 완료되고 모든 문제를 맞았는지 확인하고 XP 지급
   */
  private void checkReviewCompletionAndXp(LearningSession learningSession, StudySession reviewSession, ExamMode examMode) {
    if (!"REVIEW".equals(learningSession.getMode())) {
      return; // REVIEW 모드가 아니면 체크하지 않음
    }

    // summary 메서드에서 이미 XP 지급 로직이 처리되므로, 여기서는 제거
    // 이 메서드는 더 이상 사용하지 않거나, summary에서만 XP 지급하도록 변경
  }

  /**
   * [B] MICRO 완료 체크 및 XP 지급 (실기)
   * MINI, PRACTICAL, SUMMARY 모두 완료되었는지 확인하고 XP 지급
   */
  private void checkMicroCompletionAndXp(LearningSession learningSession, StudySession practicalSession, ExamMode examMode) {
    if (!"MICRO".equals(learningSession.getMode())) {
      return; // MICRO 모드가 아니면 체크하지 않음
    }

    // summary 메서드에서 이미 XP 지급 로직이 처리되므로, 여기서는 제거
    // 이 메서드는 더 이상 사용하지 않거나, summary에서만 XP 지급하도록 변경
  }
}
