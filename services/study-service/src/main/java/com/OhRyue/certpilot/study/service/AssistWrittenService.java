package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.QuestionChoice;
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
import com.OhRyue.certpilot.study.repository.QuestionChoiceRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.OhRyue.certpilot.study.repository.UserProgressRepository;
import com.OhRyue.certpilot.study.service.LearningSessionService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

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

  // 문제 수 5/10/20 (기존 10/20/50에서 변경)
  private static final List<Integer> ALLOWED_COUNTS = List.of(5, 10, 20);
  private static final Long ASSIST_TOPIC_ID = 0L; // 보조학습은 topicId가 없으므로 0 사용

  private final QuestionRepository questionRepository;
  private final QuestionChoiceRepository choiceRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;
  private final TopicTreeService topicTreeService;
  // StudySession 관리 및 XP 지급을 위한 의존성 추가
  private final com.OhRyue.certpilot.study.repository.UserAnswerRepository userAnswerRepository;
  private final StudySessionManager sessionManager;
  private final com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
  private final LearningSessionService learningSessionService;
  private final LearningSessionRepository learningSessionRepository;
  private final LearningStepRepository learningStepRepository;
  private final StudySessionRepository studySessionRepository;
  private final ObjectMapper objectMapper;

  /* ================= 카테고리: 토픽 배열 선택 → 해당 토픽들에서 출제 ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(List<Long> topicIds,
                                                                   Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    // 프론트엔드에서 받은 토픽 배열을 그대로 사용
    if (topicIds == null || topicIds.isEmpty()) {
      throw new IllegalArgumentException("토픽 ID 배열이 비어있습니다.");
    }

    Set<Long> topicIdSet = new HashSet<>(topicIds);

    // 1. 문제 풀 생성
    List<Question> pool = questionRepository
        .findByTopicIdInAndModeAndType(topicIdSet, ExamMode.WRITTEN, QuestionType.MCQ)
        .stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/category] topicIds={}, poolSize={}, count={}",
        topicIdSet, pool.size(), want);

    if (pool.isEmpty()) {
      throw new IllegalStateException("선택한 토픽에 문제가 없습니다.");
    }

    // 2. 문제 선택 (랜덤 셔플 후 want 개수만큼)
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);
    int lim = Math.min(copy.size(), Math.max(1, want));
    List<Question> selectedQuestions = copy.subList(0, lim);

    // 3. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_WRITTEN_CATEGORY)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_WRITTEN_CATEGORY")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 4-1. 문제 풀기 단계
    LearningStep categoryStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_WRITTEN_CATEGORY")
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

    // 5. StudySession 생성 (topicScopeJson에 topicIds 저장)
    Map<String, Object> scopeMap = new HashMap<>();
    scopeMap.put("topicIds", new ArrayList<>(topicIdSet));
    String scopeJson;
    try {
      scopeJson = objectMapper.writeValueAsString(scopeMap);
    } catch (JsonProcessingException e) {
      scopeJson = "{}";
    }

    StudySession studySession = StudySession.builder()
        .userId(userId)
        .mode("ASSIST_WRITTEN_CATEGORY")
        .examMode(ExamMode.WRITTEN)
        .topicScopeJson(scopeJson)
        .questionCount(selectedQuestions.size())
        .status("OPEN")
        .startedAt(Instant.now())
        .learningStep(categoryStep)
        .build();

    studySession = studySessionRepository.save(studySession);

    // LearningStep에 연결
    categoryStep.setStudySession(studySession);

    // 6. 문제 할당
    List<Long> questionIds = selectedQuestions.stream()
        .map(Question::getId)
        .toList();
    sessionManager.allocateQuestions(studySession, questionIds);

    // 7. 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> items = new ArrayList<>();
    for (Question q : selectedQuestions) {
      List<QuestionChoice> raw = Optional.ofNullable(
          choiceRepository.findByQuestionId(q.getId())
      ).orElse(List.of());

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

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_CATEGORY",
        "IN_PROGRESS",
        null,
        fetchStats(userId, "WRITTEN"),
        set,
        learningSession.getId()
    );
  }

  /* ================= 카테고리 문제 가져오기 ================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getCategorySet(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!"ASSIST_WRITTEN_CATEGORY".equals(learningSession.getMode())) {
      throw new IllegalStateException("카테고리 기반 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep categoryStep = learningSessionService.getStep(learningSession, "ASSIST_WRITTEN_CATEGORY");
    StudySession studySession = categoryStep.getStudySession();

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
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> quizItems = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }

          List<QuestionChoice> raw = Optional.ofNullable(
              choiceRepository.findByQuestionId(q.getId())
          ).orElse(List.of());

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

          return new AssistDtos.QuizQ(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              choices,
              q.getImageUrl()
          );
        })
        .toList();

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(quizItems);

    // 6. 단계 상태 확인
    String status = categoryStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_CATEGORY",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        set,
        learningSession.getId()
    );
  }

  /* ================= 난이도 ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(Difficulty diff,
                                                                     Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    Difficulty difficulty = (diff == null ? Difficulty.NORMAL : diff);
    int want = sanitizeCount(count);

    // 1. 문제 풀 생성
    List<Question> pool = questionRepository
        .findByModeAndTypeAndDifficulty(ExamMode.WRITTEN, QuestionType.MCQ, difficulty)
        .stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/difficulty] diff={}, poolSize={}, count={}",
        difficulty, pool.size(), want);

    if (pool.isEmpty()) {
      throw new IllegalStateException("해당 난이도의 문제가 없습니다.");
    }

    // 2. 문제 선택 (랜덤 셔플 후 want 개수만큼)
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);
    int lim = Math.min(copy.size(), Math.max(1, want));
    List<Question> selectedQuestions = copy.subList(0, lim);

    // 3. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_WRITTEN_DIFFICULTY)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_WRITTEN_DIFFICULTY")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 4-1. 문제 풀기 단계
    LearningStep difficultyStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_WRITTEN_DIFFICULTY")
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
        .mode("ASSIST_WRITTEN_DIFFICULTY")
        .examMode(ExamMode.WRITTEN)
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

    // 7. 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> items = new ArrayList<>();
    for (Question q : selectedQuestions) {
      List<QuestionChoice> raw = Optional.ofNullable(
          choiceRepository.findByQuestionId(q.getId())
      ).orElse(List.of());

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

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_DIFFICULTY",
        "IN_PROGRESS",
        null,
        fetchStats(userId, "WRITTEN"),
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
    if (!"ASSIST_WRITTEN_DIFFICULTY".equals(learningSession.getMode())) {
      throw new IllegalStateException("난이도 기반 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, "ASSIST_WRITTEN_DIFFICULTY");
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
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> quizItems = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }

          List<QuestionChoice> raw = Optional.ofNullable(
              choiceRepository.findByQuestionId(q.getId())
          ).orElse(List.of());

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

          return new AssistDtos.QuizQ(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              choices,
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
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_DIFFICULTY",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        set,
        learningSession.getId()
    );
  }

  /* ================= 약점 보완 ================= */

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    // 1. 약점 토픽 추출
    List<UserProgress> progresses = progressRepository.findByUserId(userId);
    progresses.sort(Comparator.comparingDouble(this::writtenAccuracy)
        .thenComparing(UserProgress::getUpdatedAt));

    List<Long> targetTopics = progresses.stream()
        .map(UserProgress::getTopicId)
        .limit(5)
        .toList();

    // 2. 문제 풀 생성
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

    if (pool.isEmpty()) {
      throw new IllegalStateException("약점 보완 문제가 없습니다.");
    }

    // 3. 문제 선택 (랜덤 셔플 후 want 개수만큼)
    List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
    Collections.shuffle(copy);
    int lim = Math.min(copy.size(), Math.max(1, want));
    List<Question> selectedQuestions = copy.subList(0, lim);

    // 4. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_WRITTEN_WEAKNESS)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_WRITTEN_WEAKNESS")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 5. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 5-1. 문제 풀기 단계
    LearningStep weaknessStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_WRITTEN_WEAKNESS")
        .status("IN_PROGRESS")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 5-2. 오답 정리 단계
    learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("REVIEW_WRONG")
        .status("READY")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 5-3. 결과 요약 단계
    learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("SUMMARY")
        .status("READY")
        .scorePct(null)
        .metadataJson(null)
        .createdAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 6. StudySession 생성 (topicScopeJson에 targetTopics 저장)
    Map<String, Object> scopeMap = new HashMap<>();
    scopeMap.put("targetTopics", targetTopics);
    String scopeJson;
    try {
      scopeJson = objectMapper.writeValueAsString(scopeMap);
    } catch (JsonProcessingException e) {
      scopeJson = "{}";
    }

    StudySession studySession = StudySession.builder()
        .userId(userId)
        .mode("ASSIST_WRITTEN_WEAKNESS")
        .examMode(ExamMode.WRITTEN)
        .topicScopeJson(scopeJson)
        .questionCount(selectedQuestions.size())
        .status("OPEN")
        .startedAt(Instant.now())
        .learningStep(weaknessStep)
        .build();

    studySession = studySessionRepository.save(studySession);

    // LearningStep에 연결
    weaknessStep.setStudySession(studySession);

    // 7. 문제 할당
    List<Long> questionIds = selectedQuestions.stream()
        .map(Question::getId)
        .toList();
    sessionManager.allocateQuestions(studySession, questionIds);

    // 8. 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> items = new ArrayList<>();
    for (Question q : selectedQuestions) {
      List<QuestionChoice> raw = Optional.ofNullable(
          choiceRepository.findByQuestionId(q.getId())
      ).orElse(List.of());

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

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_WEAKNESS",
        "IN_PROGRESS",
        null,
        fetchStats(userId, "WRITTEN"),
        set,
        learningSession.getId()
    );
  }

  /* ================= 약점 보완 문제 가져오기 ================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getWeaknessSet(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    if (!"ASSIST_WRITTEN_WEAKNESS".equals(learningSession.getMode())) {
      throw new IllegalStateException("약점 보완 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep weaknessStep = learningSessionService.getStep(learningSession, "ASSIST_WRITTEN_WEAKNESS");
    StudySession studySession = weaknessStep.getStudySession();

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
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // 5. 순서대로 문제 반환 (MCQ 형식으로 변환)
    List<AssistDtos.QuizQ> quizItems = items.stream()
        .sorted(Comparator.comparing(StudySessionItem::getOrderNo))
        .map(item -> {
          Question q = questionMap.get(item.getQuestionId());
          if (q == null) {
            throw new IllegalStateException("문제를 찾을 수 없습니다: " + item.getQuestionId());
          }

          List<QuestionChoice> raw = Optional.ofNullable(
              choiceRepository.findByQuestionId(q.getId())
          ).orElse(List.of());

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

          return new AssistDtos.QuizQ(
              q.getId(),
              Optional.ofNullable(q.getStem()).orElse(""),
              choices,
              q.getImageUrl()
          );
        })
        .toList();

    AssistDtos.QuizSet set = new AssistDtos.QuizSet(quizItems);

    // 6. 단계 상태 확인
    String status = weaknessStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_WEAKNESS",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        set,
        learningSession.getId()
    );
  }

  /* ================= 제출(필기 – MCQ) ================= */
  // StudySession 기록, UserAnswer 저장, 틀린 문제 수집, XP 지급

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submit(
      AssistDtos.WrittenSubmitReq req
  ) {
    String userId = AuthUserUtil.getCurrentUserId();

    if (req == null || req.answers() == null || req.answers().isEmpty()) {
      return new FlowDtos.StepEnvelope<>(
          null,
          "ASSIST_WRITTEN",
          "ASSIST_WRITTEN_SUBMIT",
          "COMPLETE",
          null,
          fetchStats(userId, "WRITTEN"),
          new AssistDtos.WrittenSubmitResp(0, 0, List.of()),
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
  private FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submitWithSession(
      AssistDtos.WrittenSubmitReq req,
      String userId
  ) {
    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(req.learningSessionId());
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    String mode = learningSession.getMode();
    if (!"ASSIST_WRITTEN_DIFFICULTY".equals(mode) && !"ASSIST_WRITTEN_WEAKNESS".equals(mode) && !"ASSIST_WRITTEN_CATEGORY".equals(mode)) {
      throw new IllegalStateException("보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    String stepCode;
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_DIFFICULTY";
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_WEAKNESS";
    } else {
      stepCode = "ASSIST_WRITTEN_CATEGORY";
    }
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, stepCode);
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
        .map(AssistDtos.WrittenAnswer::questionId)
        .filter(Objects::nonNull)
        .toList();

    Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .collect(Collectors.toMap(Question::getId, q -> q));

    List<AssistDtos.WrittenResultItem> items = new ArrayList<>();
    List<Long> wrongQuestionIds = new ArrayList<>();
    int correct = 0;

    for (AssistDtos.WrittenAnswer ans : req.answers()) {
      Question q = questionMap.get(ans.questionId());
      if (q == null) {
        continue;
      }
      String userLabel = Optional.ofNullable(ans.label()).orElse("").trim();
      String correctLabel = Optional.ofNullable(q.getAnswerKey()).orElse("").trim();
      boolean isCorrect = !correctLabel.isBlank() && correctLabel.equalsIgnoreCase(userLabel);
      if (isCorrect) {
        correct++;
      } else {
        wrongQuestionIds.add(q.getId());
      }

      // 기본 해설만 사용 (AI 해설 없음)
      String explanation = Optional.ofNullable(q.getSolutionText()).orElse("");

      items.add(new AssistDtos.WrittenResultItem(
          q.getId(),
          isCorrect,
          correctLabel,
          explanation
      ));

      // StudySessionItem 및 UserAnswer 저장
      StudySessionItem sessionItem = sessionItemMap.get(ans.questionId());
      if (sessionItem != null) {
        Map<String, Object> answerPayload = Map.of("label", userLabel, "correct", isCorrect);
        String answerJson = toJson(answerPayload);

        sessionManager.upsertItem(
            studySession, q.getId(), sessionItem.getOrderNo(), answerJson, isCorrect, isCorrect ? 100 : 0, null);

        // UserAnswer 저장
        com.OhRyue.certpilot.study.domain.UserAnswer userAnswer =
            com.OhRyue.certpilot.study.domain.UserAnswer.builder()
                .userId(userId)
                .questionId(q.getId())
                .examMode(ExamMode.WRITTEN)
                .questionType(QuestionType.MCQ)
                .userAnswerJson(answerJson)
                .correct(isCorrect)
                .score(isCorrect ? 100 : 0)
                .source("ASSIST_WRITTEN")
                .sessionId(studySession.getId())
                .sessionItemId(sessionItem.getId())
                .build();
        userAnswerRepository.save(userAnswer);
      }

      // Progress hook (통계용)
      try {
        progressHookClient.submit(
            new com.OhRyue.certpilot.study.client.ProgressHookClient.SubmitPayload(
                userId,
                ExamMode.WRITTEN.name(),
                QuestionType.MCQ.name(),
                isCorrect,
                isCorrect ? 100 : 0,
                List.of(),
                "ASSIST_WRITTEN"
            )
        );
      } catch (Exception e) {
        log.debug("Progress hook failed: {}", e.getMessage());
      }
    }

    int total = items.size();
    int scorePct = total == 0 ? 0 : (correct * 100) / total;

    // 5. LearningStep 업데이트
    Map<String, Object> metadata = new HashMap<>();
    metadata.put("total", total);
    metadata.put("correct", correct);
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

    // 6. StudySession 종료
    boolean allCorrect = !items.isEmpty() && wrongQuestionIds.isEmpty();
    if (!"SUBMITTED".equals(studySession.getStatus()) && !"CLOSED".equals(studySession.getStatus())) {
      sessionManager.closeSession(studySession, scorePct, allCorrect, metadata);
    }

    // 7. 다음 단계 결정 (오답이 있으면 REVIEW_WRONG, 없으면 SUMMARY)
    String nextStep = wrongQuestionIds.isEmpty() ? "SUMMARY" : "REVIEW_WRONG";

    // 8. 다음 단계를 IN_PROGRESS로 활성화
    if (nextStep != null) {
      var nextLearningStep = learningSessionService.getStep(learningSession, nextStep);
      if (nextLearningStep != null && "READY".equals(nextLearningStep.getStatus())) {
        nextLearningStep.setStatus("IN_PROGRESS");
        nextLearningStep.setUpdatedAt(Instant.now());
        learningStepRepository.save(nextLearningStep);
      }
    }

    // passed=true일 때만 XP 지급, 세션당 1회만
    if (allCorrect && !Boolean.TRUE.equals(studySession.getXpGranted())) {
      try {
        progressHookClient.flowComplete(
            new com.OhRyue.certpilot.study.client.ProgressHookClient.FlowCompletePayload(
                userId,
                ExamMode.WRITTEN.name(),
                "ASSIST",
                null // Assist는 topicId 없음
            )
        );
        sessionManager.markXpGranted(studySession);
      } catch (Exception e) {
        log.debug("XP grant failed for assist session {}: {}", studySession.getId(), e.getMessage());
      }
    }

    AssistDtos.WrittenSubmitResp payload =
        new AssistDtos.WrittenSubmitResp(total, correct, items);

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_SUBMIT",
        "COMPLETE",
        nextStep,
        sessionManager.loadMeta(studySession),
        payload,
        learningSession.getId()
    );
  }

  /* ================= 난이도 단건 즉시 채점 ================= */

  @Transactional
  public AssistDtos.WrittenGradeOneResp gradeOneDifficulty(Long learningSessionId, Long questionId, String label) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    String mode = learningSession.getMode();
    if (!"ASSIST_WRITTEN_DIFFICULTY".equals(mode) && !"ASSIST_WRITTEN_WEAKNESS".equals(mode) && !"ASSIST_WRITTEN_CATEGORY".equals(mode)) {
      throw new IllegalStateException("보조학습 세션이 아닙니다.");
    }

    // 2. 문제 조회
    Question question = questionRepository.findById(questionId)
        .orElseThrow(() -> new IllegalStateException("문제를 찾을 수 없습니다: " + questionId));

    if (question.getMode() != ExamMode.WRITTEN || question.getType() != QuestionType.MCQ) {
      throw new IllegalStateException("필기 MCQ 문제가 아닙니다.");
    }

    // 3. 채점
    String userLabel = Optional.ofNullable(label).orElse("").trim();
    String correctLabel = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
    boolean isCorrect = !correctLabel.isBlank() && correctLabel.equalsIgnoreCase(userLabel);
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

    // 4. StudySessionItem에 답변 저장
    String stepCode;
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_DIFFICULTY";
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_WEAKNESS";
    } else {
      stepCode = "ASSIST_WRITTEN_CATEGORY";
    }
    LearningStep difficultyStep = learningSessionService.getStep(learningSession, stepCode);
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
          answerPayload.put("label", userLabel);
          answerPayload.put("correct", isCorrect);
          String answerJson = objectMapper.writeValueAsString(answerPayload);

          sessionManager.upsertItem(
              studySession,
              questionId,
              sessionItem.getOrderNo(),
              answerJson,
              isCorrect,
              isCorrect ? 100 : 0,
              null
          );

          // UserAnswer 저장
          com.OhRyue.certpilot.study.domain.UserAnswer userAnswer =
              com.OhRyue.certpilot.study.domain.UserAnswer.builder()
                  .userId(userId)
                  .questionId(questionId)
                  .examMode(ExamMode.WRITTEN)
                  .questionType(QuestionType.MCQ)
                  .userAnswerJson(answerJson)
                  .correct(isCorrect)
                  .score(isCorrect ? 100 : 0)
                  .source("ASSIST_WRITTEN")
                  .sessionId(studySession.getId())
                  .sessionItemId(sessionItem.getId())
                  .build();
          userAnswerRepository.save(userAnswer);

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
                boolean allCorrect = wrongQuestionIds.isEmpty();
                sessionManager.closeSession(studySession, scorePct, allCorrect, metadata);
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

    return new AssistDtos.WrittenGradeOneResp(
        isCorrect,
        correctLabel,
        explanation,
        ""  // AI 해설 없음
    );
  }

  @Transactional
  private FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submitWithoutSession(
      AssistDtos.WrittenSubmitReq req,
      String userId
  ) {
    // 문제 캐시
    List<Long> qIds = req.answers().stream()
        .map(AssistDtos.WrittenAnswer::questionId)
        .filter(Objects::nonNull)
        .toList();

    Map<Long, Question> questionMap = questionRepository.findByIdIn(qIds).stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN && q.getType() == QuestionType.MCQ)
        .collect(Collectors.toMap(Question::getId, q -> q));

    // Assist용 StudySession 생성 (mode: ASSIST_CATEGORY/ASSIST_DIFFICULTY/ASSIST_WEAK)
    String assistMode = "ASSIST_CATEGORY"; // TODO: 실제로는 요청/컨트롤러에서 assistType 전달 받아야 함
    Map<String, Object> scope = Map.of("assistType", assistMode, "questionCount", req.answers().size());
    com.OhRyue.certpilot.study.domain.StudySession session = sessionManager.ensureSession(
        userId, scope, assistMode, ExamMode.WRITTEN, req.answers().size());

    List<AssistDtos.WrittenResultItem> items = new ArrayList<>();
    List<Long> wrongQuestionIds = new ArrayList<>();
    int correct = 0;

    for (int idx = 0; idx < req.answers().size(); idx++) {
      AssistDtos.WrittenAnswer ans = req.answers().get(idx);
      Question q = questionMap.get(ans.questionId());
      if (q == null) {
        continue;
      }
      int orderNo = idx + 1; // 1부터 시작하는 순서 번호
      String userLabel = Optional.ofNullable(ans.label()).orElse("").trim();
      String correctLabel = Optional.ofNullable(q.getAnswerKey()).orElse("").trim();
      boolean isCorrect = !correctLabel.isBlank() && correctLabel.equalsIgnoreCase(userLabel);
      if (isCorrect) {
        correct++;
      } else {
        wrongQuestionIds.add(q.getId());
      }

      // 기본 해설만 사용 (AI 해설 없음)
      String explanation = Optional.ofNullable(q.getSolutionText()).orElse("");

      items.add(new AssistDtos.WrittenResultItem(
          q.getId(),
          isCorrect,
          correctLabel,
          explanation
      ));

      // StudySessionItem 및 UserAnswer 저장
      Map<String, Object> answerPayload = Map.of("label", userLabel, "correct", isCorrect);
      String answerJson = toJson(answerPayload);

      com.OhRyue.certpilot.study.domain.StudySessionItem item = sessionManager.upsertItem(
          session, q.getId(), orderNo, answerJson, isCorrect, isCorrect ? 100 : 0, null);

      // UserAnswer 저장
      com.OhRyue.certpilot.study.domain.UserAnswer userAnswer =
          com.OhRyue.certpilot.study.domain.UserAnswer.builder()
              .userId(userId)
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
        progressHookClient.submit(
            new com.OhRyue.certpilot.study.client.ProgressHookClient.SubmitPayload(
                userId,
                ExamMode.WRITTEN.name(),
                QuestionType.MCQ.name(),
                isCorrect,
                isCorrect ? 100 : 0,
                List.of(),
                "ASSIST_WRITTEN"
            )
        );
      } catch (Exception e) {
        log.debug("Progress hook failed: {}", e.getMessage());
      }
    }

    // 세션 완료 처리 및 XP 지급
    boolean allCorrect = !items.isEmpty() && wrongQuestionIds.isEmpty();
    double scorePct = items.isEmpty() ? 0.0 : (correct * 100.0) / items.size();
    sessionManager.closeSession(session, scorePct, allCorrect, Map.of(
        "total", items.size(),
        "correct", correct,
        "wrongQuestionIds", wrongQuestionIds
    ));

    // passed=true일 때만 XP 지급, 세션당 1회만
    if (allCorrect && !Boolean.TRUE.equals(session.getXpGranted())) {
      try {
        progressHookClient.flowComplete(
            new com.OhRyue.certpilot.study.client.ProgressHookClient.FlowCompletePayload(
                userId,
                ExamMode.WRITTEN.name(),
                "ASSIST",
                null // Assist는 topicId 없음
            )
        );
        sessionManager.markXpGranted(session);
      } catch (Exception e) {
        log.debug("XP grant failed for assist session {}: {}", session.getId(), e.getMessage());
      }
    }

    AssistDtos.WrittenSubmitResp payload =
        new AssistDtos.WrittenSubmitResp(req.answers().size(), correct, items);

    return new FlowDtos.StepEnvelope<>(
        session.getId(), // sessionId 반환 (틀린 문제 다시보기용)
        "ASSIST_WRITTEN",
        "ASSIST_WRITTEN_SUBMIT",
        "COMPLETE",
        null,
        fetchStats(userId, "WRITTEN"),
        payload,
        null
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

  /** 5/10/20 중 가장 가까운 값으로 보정 (미지정/이상치 방지) */
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
        payload,
        null  // ASSIST는 LearningSession을 사용하지 않음
    );
  }

  private Map<String, Object> fetchStats(String userId, String reportMode) {
    Map<String, Object> meta = new HashMap<>();
    if (userId == null || userId.isBlank()) {
      return meta;
    }

    // 오늘 보조학습 목표 (JWT 기반)
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

    // WRITTEN / PRACTICAL 모드 리포트 개요
    try {
      ProgressQueryClient.Overview overview = progressQueryClient.overview(reportMode);
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
