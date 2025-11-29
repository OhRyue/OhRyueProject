package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.OhRyue.certpilot.study.service.LearningSessionService;
import com.OhRyue.certpilot.study.service.StudySessionManager;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 보조학습 - 실기(Short/Long) 전용 서비스
 * - 카테고리/난이도/약점 보완 세트 생성
 * - 제출 시 AIExplanationService 를 이용해 단순 채점
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AssistPracticalService {

  private static final List<Integer> ALLOWED_COUNTS = List.of(5, 10, 20);
  private static final Long ASSIST_TOPIC_ID = 0L; // 보조학습은 topicId가 없으므로 0 사용
  private final QuestionRepository questionRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;
  private final TopicTreeService topicTreeService;
  private final AIExplanationService aiExplanationService;
  private final LearningSessionService learningSessionService;
  private final StudySessionManager sessionManager;
  private final LearningSessionRepository learningSessionRepository;
  private final LearningStepRepository learningStepRepository;
  private final StudySessionRepository studySessionRepository;
  private final com.OhRyue.certpilot.study.repository.UserAnswerRepository userAnswerRepository;
  private final com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
  private final ObjectMapper objectMapper;

  /* ================= 카테고리: rootTopicId 기준 하위 토픽 전체 ================= */

  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(Long rootTopicId,
                                                                   Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    Set<Long> topicIds = topicTreeService.descendantsOf(rootTopicId);
    if (topicIds.isEmpty()) {
      topicIds = Set.of(rootTopicId);
    }

    List<Question> pool = new ArrayList<>();
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.SHORT));
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(topicIds, ExamMode.PRACTICAL, QuestionType.LONG));

    pool = pool.stream()
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/practical/category] rootTopicId={}, topicIds={}, poolSize={}, count={}",
        rootTopicId, topicIds, pool.size(), want);

    AssistDtos.QuizSet set = pickPractical(pool, want);
    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_CATEGORY", userId, set);
  }

  /* ================= 난이도 ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(Difficulty diff,
                                                                     Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    Difficulty difficulty = (diff == null ? Difficulty.NORMAL : diff);
    int want = sanitizeCount(count);

    // 1. 문제 풀 생성
    List<Question> pool = Stream.concat(
            questionRepository.findByModeAndTypeAndDifficulty(
                ExamMode.PRACTICAL, QuestionType.SHORT, difficulty).stream(),
            questionRepository.findByModeAndTypeAndDifficulty(
                ExamMode.PRACTICAL, QuestionType.LONG, difficulty).stream()
        )
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/practical/difficulty] diff={}, poolSize={}, count={}",
        difficulty, pool.size(), want);

    if (pool.isEmpty()) {
      throw new IllegalStateException("해당 난이도의 문제가 없습니다.");
    }

    // 2. 문제 선택 (랜덤 셔플 후 want 개수만큼)
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);
    int lim = Math.min(copy.size(), Math.max(1, want));
    List<Question> selectedQuestions = copy.subList(0, lim);

    // 3. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_PRACTICAL_DIFFICULTY)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_PRACTICAL_DIFFICULTY")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 4-1. 문제 풀기 단계
    LearningStep difficultyStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_PRACTICAL_DIFFICULTY")
        .status("IN_PROGRESS")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4-2. 오답 정리 단계
    learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("REVIEW_WRONG")
        .status("READY")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4-3. 결과 요약 단계
    learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("SUMMARY")
        .status("READY")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 5. StudySession 생성 (topicScopeJson에 difficulty 저장)
    Map<String, Object> scopeMap = new HashMap<>();
    scopeMap.put("difficulty", difficulty.name());
    String scopeJson;
    try {
      scopeJson = objectMapper.writeValueAsString(scopeMap);
    } catch (JsonProcessingException e) {
      scopeJson = "{}";
    }

    StudySession studySession = StudySession.builder()
        .userId(userId)
        .mode("ASSIST_PRACTICAL_DIFFICULTY")
        .examMode(ExamMode.PRACTICAL)
        .topicScopeJson(scopeJson)
        .questionCount(selectedQuestions.size())
        .status("OPEN")
        .startedAt(Instant.now())
        .learningStep(difficultyStep)
        .build();

    studySession = studySessionRepository.save(studySession);

    // LearningStep에 연결
    difficultyStep.setStudySession(studySession);

    // 6. 문제 할당
    List<Long> questionIds = selectedQuestions.stream()
        .map(Question::getId)
        .toList();
    sessionManager.allocateQuestions(studySession, questionIds);

    // 7. 문제 반환
    List<AssistDtos.QuizQ> items = selectedQuestions.stream()
        .map(q -> new AssistDtos.QuizQ(
            q.getId(),
            Optional.ofNullable(q.getStem()).orElse(""),
            List.of(), // 실기는 선택지 없음
            q.getImageUrl()
        ))
        .toList();

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_DIFFICULTY",
        "IN_PROGRESS",
        null,
        fetchStats(userId),
        set,
        learningSession.getId()
    );
  }

  /* ================= 난이도 문제 가져오기 ================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getDifficultySet(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(learningSession.getMode())) {
      throw new IllegalStateException("난이도 기반 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, "ASSIST_PRACTICAL_DIFFICULTY");
    StudySession studySession = difficultyStep.getStudySession();

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
    List<AssistDtos.QuizQ> quizItems = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }
          return new AssistDtos.QuizQ(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              List.of(), // 실기는 선택지 없음
              q.getImageUrl()
          );
        })
        .toList();

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(quizItems);

    // 6. 단계 상태 확인
    String status = difficultyStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_DIFFICULTY",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        set,
        learningSession.getId()
    );
  }

  /* ================= 약점 보완 ================= */

  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    List<UserProgress> ps = progressRepository.findByUserId(userId);
    ps.sort(Comparator.comparingDouble(this::avgScore)
        .thenComparing(UserProgress::getUpdatedAt));
    List<Long> targetTopics = ps.stream()
        .map(UserProgress::getTopicId)
        .limit(5)
        .toList();

    List<Question> pool;
    if (targetTopics.isEmpty()) {
      pool = Stream.concat(
              questionRepository.findByModeAndTypeAndDifficulty(
                  ExamMode.PRACTICAL, QuestionType.SHORT, Difficulty.NORMAL).stream(),
              questionRepository.findByModeAndTypeAndDifficulty(
                  ExamMode.PRACTICAL, QuestionType.LONG, Difficulty.NORMAL).stream()
          )
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    } else {
      pool = new ArrayList<>();
      pool.addAll(questionRepository
          .findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.SHORT));
      pool.addAll(questionRepository
          .findByTopicIdInAndModeAndType(targetTopics, ExamMode.PRACTICAL, QuestionType.LONG));
      pool = pool.stream()
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    }

    log.debug("[assist/practical/weakness] userId={}, targets={}, poolSize={}, count={}",
        userId, targetTopics, pool.size(), want);

    AssistDtos.QuizSet set = pickPractical(pool, want);
    return wrap("ASSIST_PRACTICAL", "ASSIST_PRACTICAL_WEAKNESS", userId, set);
  }

  /* ================= 제출(실기 – SHORT/LONG, LLM 채점) ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submit(
      AssistDtos.PracticalSubmitReq req
  ) {
    String userId = AuthUserUtil.getCurrentUserId();

    if (req == null || req.answers() == null || req.answers().isEmpty()) {
      return new FlowDtos.StepEnvelope<>(
          null,
          "ASSIST_PRACTICAL",
          "ASSIST_PRACTICAL_SUBMIT",
          "COMPLETE",
          null,
          fetchStats(userId),
          new AssistDtos.PracticalSubmitResp(0, 0, List.of()),
          req != null ? req.learningSessionId() : null
      );
    }

    // learningSessionId가 있으면 세션 기반 처리
    if (req.learningSessionId() != null) {
      return submitWithSession(req, userId);
    }

    // 기존 방식 (세션 없이 처리)
    return submitWithoutSession(req, userId);
  }

  @Transactional
  private FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submitWithSession(
      AssistDtos.PracticalSubmitReq req,
      String userId
  ) {
    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(req.learningSessionId());
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(learningSession.getMode())) {
      throw new IllegalStateException("난이도 기반 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, "ASSIST_PRACTICAL_DIFFICULTY");
    StudySession studySession = difficultyStep.getStudySession();

    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    // 3. 세션에 할당된 문제 조회
    List<StudySessionItem> sessionItems = sessionManager.items(studySession.getId());
    Map<Long, StudySessionItem> sessionItemMap = sessionItems.stream()
        .collect(Collectors.toMap(StudySessionItem::getQuestionId, item -> item));

    // 4. 문제 조회
    List<Long> qIds = req.answers().stream()
        .map(AssistDtos.PracticalAnswer::questionId)
        .filter(Objects::nonNull)
        .toList();

    Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 채점 및 결과 저장
    List<AssistDtos.PracticalResultItem> items = new ArrayList<>();
    int correctCount = 0;
    int graded = 0;

    for (AssistDtos.PracticalAnswer ans : req.answers()) {
      Question q = questionMap.get(ans.questionId());
      if (q == null) {
        continue;
      }

      AIExplanationService.PracticalResult result =
          aiExplanationService.explainAndScorePractical(q, ans.userText());

      boolean correct = Optional.ofNullable(result.correct()).orElse(false);
      if (correct) {
        correctCount++;
      }
      graded++;

      String baseExplain = Optional.ofNullable(q.getSolutionText()).orElse("");
      String aiExplain = result.explain();

      items.add(new AssistDtos.PracticalResultItem(
          q.getId(),
          correct,
          baseExplain,
          aiExplain
      ));

      // StudySessionItem에 답변 저장
      StudySessionItem sessionItem = sessionItemMap.get(ans.questionId());
      if (sessionItem != null) {
        try {
          String answerJson = objectMapper.writeValueAsString(Map.of("userText", ans.userText()));
          String aiExplainJson = objectMapper.writeValueAsString(Map.of(
              "explain", aiExplain,
              "correct", correct
          ));
          sessionManager.upsertItem(
              studySession,
              ans.questionId(),
              sessionItem.getOrderNo(),
              answerJson,
              correct,
              correct ? 100 : 0,
              aiExplainJson
          );
        } catch (JsonProcessingException e) {
          log.warn("Failed to serialize answer for question {}: {}", ans.questionId(), e.getMessage());
        }
      }
    }

    int total = graded;
    int scorePct = total == 0 ? 0 : (correctCount * 100) / total;

    // 6. 오답 문제 ID 수집
    List<Long> wrongQuestionIds = items.stream()
        .filter(item -> !item.correct())
        .map(AssistDtos.PracticalResultItem::questionId)
        .toList();

    // 7. LearningStep 업데이트
    Map<String, Object> metadata = new HashMap<>();
    metadata.put("total", total);
    metadata.put("correct", correctCount);
    metadata.put("completed", true);
    metadata.put("wrongQuestionIds", wrongQuestionIds);
    metadata.put("lastSubmittedAt", Instant.now().toString());

    try {
      difficultyStep.setMetadataJson(objectMapper.writeValueAsString(metadata));
    } catch (JsonProcessingException e) {
      log.warn("Failed to serialize metadata: {}", e.getMessage());
    }
    difficultyStep.setScorePct(scorePct);
    difficultyStep.setStatus("COMPLETE");
    difficultyStep.setUpdatedAt(Instant.now());
    learningStepRepository.save(difficultyStep);

    // 8. StudySession 종료
    if (!"SUBMITTED".equals(studySession.getStatus()) && !"CLOSED".equals(studySession.getStatus())) {
      sessionManager.closeSession(studySession, scorePct, metadata);
    }

    // 9. 다음 단계 결정 (오답이 있으면 REVIEW_WRONG, 없으면 SUMMARY)
    String nextStep = wrongQuestionIds.isEmpty() ? "SUMMARY" : "REVIEW_WRONG";

    // 10. 다음 단계를 IN_PROGRESS로 활성화
    if (nextStep != null) {
      var nextLearningStep = learningSessionService.getStep(learningSession, nextStep);
      if (nextLearningStep != null && "READY".equals(nextLearningStep.getStatus())) {
        nextLearningStep.setStatus("IN_PROGRESS");
        nextLearningStep.setUpdatedAt(Instant.now());
        learningStepRepository.save(nextLearningStep);
      }
    }

    AssistDtos.PracticalSubmitResp payload =
        new AssistDtos.PracticalSubmitResp(total, correctCount, items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_SUBMIT",
        "COMPLETE",
        nextStep,
        sessionManager.loadMeta(studySession),
        payload,
        learningSession.getId()
    );
  }

  /* ================= 난이도 단건 즉시 채점 ================= */

  @Transactional
  public AssistDtos.PracticalGradeOneResp gradeOneDifficulty(Long learningSessionId, Long questionId, String userText) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(learningSession.getMode())) {
      throw new IllegalStateException("난이도 기반 보조학습 세션이 아닙니다.");
    }

    // 2. 문제 조회
    Question question = questionRepository.findById(questionId)
        .orElseThrow(() -> new IllegalStateException("문제를 찾을 수 없습니다: " + questionId));

    if (question.getMode() != ExamMode.PRACTICAL) {
      throw new IllegalStateException("실기 문제가 아닙니다.");
    }

    // 3. AI 채점
    AIExplanationService.PracticalResult result =
        aiExplanationService.explainAndScorePractical(question, userText);

    boolean correct = Optional.ofNullable(result.correct()).orElse(false);
    String baseExplain = Optional.ofNullable(question.getSolutionText()).orElse("");
    String aiExplain = result.explain();

    // 4. StudySessionItem에 답변 저장
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, "ASSIST_PRACTICAL_DIFFICULTY");
    StudySession studySession = difficultyStep.getStudySession();

    if (studySession != null) {
      List<StudySessionItem> sessionItems = sessionManager.items(studySession.getId());
      StudySessionItem sessionItem = sessionItems.stream()
          .filter(item -> item.getQuestionId().equals(questionId))
          .findFirst()
          .orElse(null);

      if (sessionItem != null) {
        try {
          Map<String, Object> answerPayload = new HashMap<>();
          answerPayload.put("answer", userText);
          answerPayload.put("correct", correct);
          answerPayload.put("tips", result.tips());
          String answerJson = objectMapper.writeValueAsString(answerPayload);

          Map<String, Object> aiExplainMap = new HashMap<>();
          aiExplainMap.put("explain", aiExplain);
          aiExplainMap.put("tips", result.tips());
          aiExplainMap.put("aiFailed", result.aiFailed());
          String aiExplainJson = objectMapper.writeValueAsString(aiExplainMap);

          sessionManager.upsertItem(
              studySession,
              questionId,
              sessionItem.getOrderNo(),
              answerJson,
              correct,
              correct ? 100 : 0,
              aiExplainJson
          );

          // UserAnswer 저장
          com.OhRyue.certpilot.study.domain.UserAnswer userAnswer =
              com.OhRyue.certpilot.study.domain.UserAnswer.builder()
                  .userId(userId)
                  .questionId(questionId)
                  .examMode(ExamMode.PRACTICAL)
                  .questionType(question.getType())
                  .userAnswerJson(answerJson)
                  .correct(correct)
                  .score(correct ? 100 : 0)
                  .source("ASSIST_PRACTICAL")
                  .sessionId(studySession.getId())
                  .sessionItemId(sessionItem.getId())
                  .build();
          userAnswerRepository.save(userAnswer);

          // Progress hook (통계용)
          try {
            progressHookClient.submit(
                new com.OhRyue.certpilot.study.client.ProgressHookClient.SubmitPayload(
                    userId,
                    ExamMode.PRACTICAL.name(),
                    question.getType().name(),
                    correct,
                    correct ? 100 : 0,
                    List.of(),
                    "ASSIST_PRACTICAL"
                )
            );
          } catch (Exception e) {
            log.debug("Progress hook failed: {}", e.getMessage());
          }

          // 5. 메타데이터 업데이트 (모든 문제를 풀었는지 확인)
          List<StudySessionItem> allItems = sessionManager.items(studySession.getId());
          long answeredCount = allItems.stream()
              .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
              .count();
          long totalCount = allItems.size();
          
          // 모든 문제를 풀었으면 메타데이터 업데이트
          if (answeredCount == totalCount && totalCount > 0) {
            int correctCount = (int) allItems.stream()
                .filter(item -> Boolean.TRUE.equals(item.getCorrect()))
                .count();
            List<Long> wrongQuestionIds = allItems.stream()
                .filter(item -> Boolean.FALSE.equals(item.getCorrect()))
                .map(StudySessionItem::getQuestionId)
                .toList();
            
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("total", (int) totalCount);
            metadata.put("correct", correctCount);
            metadata.put("completed", true);
            metadata.put("wrongQuestionIds", wrongQuestionIds);
            metadata.put("lastSubmittedAt", Instant.now().toString());
            
            try {
              difficultyStep.setMetadataJson(objectMapper.writeValueAsString(metadata));
              int scorePct = totalCount == 0 ? 0 : (correctCount * 100) / (int) totalCount;
              difficultyStep.setScorePct(scorePct);
              difficultyStep.setStatus("COMPLETE");
              difficultyStep.setUpdatedAt(Instant.now());
              learningStepRepository.save(difficultyStep);
              
              // StudySession 종료
              if (!"SUBMITTED".equals(studySession.getStatus()) && !"CLOSED".equals(studySession.getStatus())) {
                sessionManager.closeSession(studySession, scorePct, metadata);
              }
              
              // 다음 단계 결정 및 활성화
              String nextStep = wrongQuestionIds.isEmpty() ? "SUMMARY" : "REVIEW_WRONG";
              var nextLearningStep = learningSessionService.getStep(learningSession, nextStep);
              if (nextLearningStep != null && "READY".equals(nextLearningStep.getStatus())) {
                nextLearningStep.setStatus("IN_PROGRESS");
                nextLearningStep.setUpdatedAt(Instant.now());
                learningStepRepository.save(nextLearningStep);
              }
            } catch (JsonProcessingException e) {
              log.warn("Failed to update metadata: {}", e.getMessage());
            }
          }
        } catch (JsonProcessingException e) {
          log.warn("Failed to serialize answer for question {}: {}", questionId, e.getMessage());
        }
      }
    }

    return new AssistDtos.PracticalGradeOneResp(
        correct,
        Optional.ofNullable(question.getAnswerKey()).orElse(""),
        baseExplain,
        aiExplain,
        result.aiFailed()
    );
  }

  @Transactional
  private FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submitWithoutSession(
      AssistDtos.PracticalSubmitReq req,
      String userId
  ) {
    List<Long> qIds = req.answers().stream()
        .map(AssistDtos.PracticalAnswer::questionId)
        .filter(Objects::nonNull)
        .toList();

    Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .collect(Collectors.toMap(Question::getId, q -> q));

    List<AssistDtos.PracticalResultItem> items = new ArrayList<>();
    int correctCount = 0;
    int graded = 0;

    for (AssistDtos.PracticalAnswer ans : req.answers()) {
      Question q = questionMap.get(ans.questionId());
      if (q == null) {
        continue;
      }

      AIExplanationService.PracticalResult result =
          aiExplanationService.explainAndScorePractical(q, ans.userText());

      boolean correct = Optional.ofNullable(result.correct()).orElse(false);
      if (correct) {
        correctCount++;
      }
      graded++;

      String baseExplain = Optional.ofNullable(q.getSolutionText()).orElse("");
      String aiExplain = result.explain();

      items.add(new AssistDtos.PracticalResultItem(
          q.getId(),
          correct,
          baseExplain,
          aiExplain
      ));
    }

    int total = graded;

    AssistDtos.PracticalSubmitResp payload =
        new AssistDtos.PracticalSubmitResp(total, correctCount, items);

    return new FlowDtos.StepEnvelope<>(
        null,
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_SUBMIT",
        "COMPLETE",
        null,
        fetchStats(userId),
        payload,
        null
    );
  }

  /* ================= 내부 유틸 ================= */

  private AssistDtos.QuizSet pickPractical(List<Question> pool, int count) {
    if (pool == null || pool.isEmpty()) {
      return new AssistDtos.QuizSet(List.of());
    }

    // 중복 제거 후 셔플
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);

    int lim = Math.min(copy.size(), Math.max(1, count));

    List<AssistDtos.QuizQ> items = copy.subList(0, lim).stream()
        .map(q -> new AssistDtos.QuizQ(
            q.getId(),
            Optional.ofNullable(q.getStem()).orElse(""),
            List.of(), // 실기는 선택지 없음
            q.getImageUrl()
        ))
        .toList();

    return new AssistDtos.QuizSet(items);
  }

  private double avgScore(UserProgress p) {
    return Optional.ofNullable(p.getPracticalAvgScore()).orElse(0.0);
  }

  /** 10/20/50 중 가장 가까운 값으로 보정 (미지정/이상치 방지) */
  private int sanitizeCount(Integer v) {
    if (v == null) return 20;
    if (ALLOWED_COUNTS.contains(v)) return v;
    return ALLOWED_COUNTS.stream()
        .min(Comparator.comparingInt(a -> Math.abs(a - v)))
        .orElse(20);
  }

  private FlowDtos.StepEnvelope<AssistDtos.QuizSet> wrap(String mode,
                                                         String step,
                                                         String userId,
                                                         AssistDtos.QuizSet payload) {
    Map<String, Object> meta = fetchStats(userId);
    return new FlowDtos.StepEnvelope<>(
        null,
        mode,
        step,
        "IN_PROGRESS",
        null,
        meta,
        payload,
        null  // ASSIST는 LearningSession을 사용하지 않음
    );
  }

  private Map<String, Object> fetchStats(String userId) {
    Map<String, Object> meta = new HashMap<>();
    if (userId == null || userId.isBlank()) {
      return meta;
    }

    // 오늘 보조학습 목표 (JWT 기반, userId는 서버에서 해석)
    try {
      ProgressQueryClient.GoalToday goal = progressQueryClient.getTodayGoal();
      if (goal != null) {
        meta.put("todayGoal", Map.of(
            "target", goal.target(),
            "progress", goal.progress(),
            "remaining", goal.remaining(),
            "completed", goal.completed(),
            "date", goal.date(),
            "updatedAt", goal.updatedAt()
        ));
      }
    } catch (Exception ex) {
      log.debug("Failed to fetch today goal for {}: {}", userId, ex.getMessage());
    }

    // PRACTICAL 모드 리포트 개요
    try {
      ProgressQueryClient.Overview overview = progressQueryClient.overview("PRACTICAL");
      if (overview != null) {
        meta.put("weeklySolved", overview.problemsThisWeek());
        meta.put("avgAccuracy", overview.avgAccuracy());
      }
    } catch (Exception ex) {
      log.debug("Failed to fetch practical overview for {}: {}", userId, ex.getMessage());
    }

    return meta;
  }
}
