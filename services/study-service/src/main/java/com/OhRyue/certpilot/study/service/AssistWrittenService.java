package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.client.ProgressXpClient;
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

  // 약점 모드 상수
  private static final int MAX_WEAK_TAGS = 5; // 최대 사용 약점 태그 개수
  private static final int MIN_TAGS_IN_SET = 3; // 한 세트에 최소로 섞이고 싶은 태그 개수
  private static final double MAX_SHARE_PER_TAG = 0.5; // 한 태그가 전체 세트에서 차지하는 최대 비율 (50%)
  private static final int MIN_TOTAL_TRIES = 3; // 태그 능력지수 신뢰를 위한 최소 풀이 수

  // 약점 태그 정보를 담는 내부 DTO
  private record TagAbility(String tag, double accuracy, int total) {}

  private final QuestionRepository questionRepository;
  private final QuestionChoiceRepository choiceRepository;
  private final UserProgressRepository progressRepository;
  private final ProgressQueryClient progressQueryClient;
  private final TopicTreeService topicTreeService;
  // StudySession 관리 및 XP 지급을 위한 의존성 추가
  private final com.OhRyue.certpilot.study.repository.UserAnswerRepository userAnswerRepository;
  private final com.OhRyue.certpilot.study.repository.QuestionTagRepository questionTagRepository;
  private final StudySessionManager sessionManager;
  private final com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
  private final LearningSessionService learningSessionService;
  private final LearningSessionRepository learningSessionRepository;
  private final LearningStepRepository learningStepRepository;
  private final StudySessionRepository studySessionRepository;
  private final ObjectMapper objectMapper;
  private final AIExplanationService aiExplanationService;
  private final com.OhRyue.certpilot.study.client.ProgressXpClient progressXpClient;

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

    // 1. 각 토픽에 대해 자식 토픽 확인 및 최종 토픽 ID 집합 생성
    // 2레벨 토픽(자식이 있는 경우)은 자식 토픽들을 사용, 3레벨 토픽(자식이 없는 경우)은 원래 토픽 사용
    Set<Long> finalTopicIds = new HashSet<>();
    Map<Long, Set<Long>> topicToChildMap = new HashMap<>(); // 원본 토픽 -> 실제 사용할 토픽들 매핑
    
    for (Long topicId : topicIds) {
      Set<Long> children = topicTreeService.childrenOf(topicId, "WRITTEN");
      if (children != null && !children.isEmpty()) {
        // 2레벨 토픽: 자식 토픽들을 사용
        finalTopicIds.addAll(children);
        topicToChildMap.put(topicId, children);
      } else {
        // 3레벨 토픽 또는 자식이 없는 토픽: 원래 토픽 사용
        finalTopicIds.add(topicId);
        topicToChildMap.put(topicId, Set.of(topicId));
      }
    }

    log.debug("[assist/written/category] originalTopicIds={}, finalTopicIds={}, topicToChildMap={}",
        topicIds, finalTopicIds, topicToChildMap);

    // 2. 문제 풀 생성 (최종 토픽 ID 집합으로 조회)
    List<Question> pool = questionRepository
        .findByTopicIdInAndModeAndType(finalTopicIds, ExamMode.WRITTEN, QuestionType.MCQ)
        .stream()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/written/category] poolSize={}, count={}", pool.size(), want);

    if (pool.isEmpty()) {
      throw new IllegalStateException("선택한 토픽에 문제가 없습니다.");
    }

    // 3. 문제를 실제 토픽 ID별로 그룹화 (자식 토픽별로 그룹화)
    Map<Long, List<Question>> questionsByTopicId = new HashMap<>();
    for (Question q : pool) {
      questionsByTopicId
          .computeIfAbsent(q.getTopicId(), k -> new ArrayList<>())
          .add(q);
    }

    // 4. 각 실제 토픽에서 골고루 문제 선택 (균등 분배)
    List<Question> selectedQuestions = new ArrayList<>();
    
    // 각 토픽의 문제를 섞기
    questionsByTopicId.values().forEach(Collections::shuffle);
    
    // 실제 토픽 ID 목록 (자식 토픽들)
    List<Long> topicIdsList = new ArrayList<>(questionsByTopicId.keySet());
    int topicCount = topicIdsList.size();
    
    if (topicCount == 0) {
      throw new IllegalStateException("선택 가능한 토픽이 없습니다.");
    }
    
    // 기본 할당량: 각 토픽에 동일하게 할당
    int basePerTopic = want / topicCount;
    int remainder = want % topicCount; // 나머지는 첫 몇 개 토픽에 추가 할당
    
    Map<Long, Integer> topicQuotas = new HashMap<>(); // 각 토픽에 할당할 문제 수
    for (int i = 0; i < topicIdsList.size(); i++) {
      Long topicId = topicIdsList.get(i);
      int quota = basePerTopic + (i < remainder ? 1 : 0);
      topicQuotas.put(topicId, quota);
    }
    
    log.debug("[assist/written/category] topicQuotas={}, want={}, topicCount={}, topicIdsList={}", 
        topicQuotas, want, topicCount, topicIdsList);
    
    // 각 토픽에서 할당량만큼 선택
    for (Long topicId : topicIdsList) {
      List<Question> topicQuestions = questionsByTopicId.get(topicId);
      if (topicQuestions == null || topicQuestions.isEmpty()) {
        continue;
      }
      
      int quota = topicQuotas.getOrDefault(topicId, 0);
      int toSelect = Math.min(quota, topicQuestions.size());
      
      for (int i = 0; i < toSelect && selectedQuestions.size() < want; i++) {
        Question q = topicQuestions.get(i);
        if (!selectedQuestions.contains(q)) {
          selectedQuestions.add(q);
        }
      }
    }
    
    // 5. 할당량으로 부족한 경우 (일부 토픽에 문제가 부족한 경우), 
    // 문제가 많은 토픽에서 추가로 선택
    if (selectedQuestions.size() < want) {
      int remaining = want - selectedQuestions.size();
      
      // 토픽별 선택된 문제 수를 계산
      Map<Long, Integer> selectedCount = new HashMap<>();
      for (Question q : selectedQuestions) {
        selectedCount.put(q.getTopicId(), selectedCount.getOrDefault(q.getTopicId(), 0) + 1);
      }
      
      // 문제가 많은 토픽부터 추가 선택 (현재 선택 비율이 낮은 토픽 우선)
      List<Map.Entry<Long, List<Question>>> sortedTopics = questionsByTopicId.entrySet().stream()
          .sorted((e1, e2) -> {
            int size1 = e1.getValue().size();
            int size2 = e2.getValue().size();
            int selected1 = selectedCount.getOrDefault(e1.getKey(), 0);
            int selected2 = selectedCount.getOrDefault(e2.getKey(), 0);
            // 남은 문제 수가 많고, 선택 비율이 낮은 토픽 우선
            int ratio1 = size1 > 0 ? (selected1 * 100) / size1 : 0;
            int ratio2 = size2 > 0 ? (selected2 * 100) / size2 : 0;
            if (ratio1 != ratio2) {
              return Integer.compare(ratio1, ratio2); // 낮은 비율 우선
            }
            return Integer.compare(size2 - selected2, size1 - selected1); // 남은 문제가 많은 순
          })
          .toList();
      
      for (Map.Entry<Long, List<Question>> entry : sortedTopics) {
        if (remaining <= 0) break;
        
        Long topicId = entry.getKey();
        List<Question> topicQuestions = entry.getValue();
        
        for (Question q : topicQuestions) {
          if (remaining <= 0) break;
          if (!selectedQuestions.contains(q)) {
            selectedQuestions.add(q);
            remaining--;
          }
        }
      }
    }
    
    // 6. 최종적으로 부족하면 전체 풀에서 랜덤으로 추가
    if (selectedQuestions.size() < want) {
      List<Question> remainingPool = new ArrayList<>(pool);
      remainingPool.removeAll(selectedQuestions);
      Collections.shuffle(remainingPool);
      
      int additional = Math.min(want - selectedQuestions.size(), remainingPool.size());
      selectedQuestions.addAll(remainingPool.subList(0, additional));
    }

    log.debug("[assist/written/category] selectedQuestions={}, count={}", 
        selectedQuestions.size(), selectedQuestions.size());

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

    // 6. StudySession 생성 (topicScopeJson에 원본 topicIds 저장)
    Map<String, Object> scopeMap = new HashMap<>();
    scopeMap.put("topicIds", new ArrayList<>(topicIds)); // 원본 토픽 ID 배열 저장
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

    // 1. 약점 태그 후보 선택 (ability-by-tag 기반)
    List<TagAbility> weakTagCandidates = pickWeakTags(userId);
    
    log.debug("[assist/written/weakness] userId={}, weakTagCandidates={}", 
        userId, weakTagCandidates.stream().map(TagAbility::tag).toList());

    // 2. 문제 풀 생성
    List<Question> selectedQuestions;
    if (weakTagCandidates.isEmpty()) {
      // 약점 태그가 없을 때는 NORMAL 난이도 문제를 찾음
      List<Question> pool = questionRepository
          .findByModeAndTypeAndDifficulty(ExamMode.WRITTEN, QuestionType.MCQ, Difficulty.NORMAL);
      List<Question> copy = new ArrayList<>(new LinkedHashSet<>(pool));
      Collections.shuffle(copy);
      int lim = Math.min(copy.size(), Math.max(1, want));
      selectedQuestions = copy.subList(0, lim);
    } else {
      // 2-1. 태그별 문제 풀 준비
      Map<String, List<Question>> questionsByTag = new HashMap<>();
      for (TagAbility tagAbility : weakTagCandidates) {
        String tag = tagAbility.tag();
        List<Long> questionIds = questionTagRepository.findQuestionIdsByTag(tag);
        List<Question> tagQuestions = questionRepository.findByIdIn(questionIds).stream()
            .filter(q -> q.getMode() == ExamMode.WRITTEN)
            .filter(q -> q.getType() == QuestionType.MCQ)
            .distinct()
            .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
        Collections.shuffle(tagQuestions);
        questionsByTag.put(tag, tagQuestions);
      }
      
      log.debug("[assist/written/weakness] questionsByTag sizes={}", 
          questionsByTag.entrySet().stream()
              .collect(java.util.stream.Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size())));
      
      // 2-2. 약점도 기반 태그별 문제 개수 배분
      Map<String, Integer> tagAllocation = allocateCountPerTag(weakTagCandidates, want);
      
      log.debug("[assist/written/weakness] tagAllocation={}, want={}", tagAllocation, want);
      
      // 2-3. 태그별로 실제 문제 선택
      selectedQuestions = pickQuestionsByTagAllocation(weakTagCandidates, tagAllocation, questionsByTag, want);
      
      // 2-4. 태그 다양성 보정
      selectedQuestions = ensureTagDiversity(selectedQuestions, weakTagCandidates, want);
    }

    // 최종 검증: WRITTEN 모드 문제만 포함되도록 필터링
    selectedQuestions = selectedQuestions.stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN)
        .filter(q -> q.getType() == QuestionType.MCQ)
        .distinct()
        .collect(java.util.stream.Collectors.toCollection(ArrayList::new));

    log.debug("[assist/written/weakness] userId={}, selectedQuestions.size()={}, count={}",
        userId, selectedQuestions.size(), want);

    if (selectedQuestions.isEmpty()) {
      throw new IllegalStateException("약점 보완 문제가 없습니다.");
    }

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

    // 6. StudySession 생성 (topicScopeJson에 weaknessTags 저장)
    Map<String, Object> scopeMap = new HashMap<>();
    List<String> weaknessTagNames = weakTagCandidates.stream()
        .map(TagAbility::tag)
        .toList();
    scopeMap.put("weaknessTags", weaknessTagNames);
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

    // 보조학습은 각 문제 제출 시 이미 progressHookClient.submit()을 통해 XP가 지급됨
    // flowComplete는 메인학습(MICRO/REVIEW)용이므로 호출하지 않음

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

  /* ================= 보조학습 요약 ================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<AssistDtos.WrittenSummaryResp> summary(Long learningSessionId) {
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

    // 2. 문제 풀기 단계 조회
    String stepCode;
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_DIFFICULTY";
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_WEAKNESS";
    } else {
      stepCode = "ASSIST_WRITTEN_CATEGORY";
    }
    
    LearningStep assistStep = learningSessionService.getStep(learningSession, stepCode);
    if (assistStep == null) {
      throw new IllegalStateException("학습 단계를 찾을 수 없습니다.");
    }

    StudySession studySession = assistStep.getStudySession();
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    // 3. 메타데이터 추출
    Map<String, Object> metadata = parseJson(assistStep.getMetadataJson());
    int total = readInt(metadata, "total");
    int correct = readInt(metadata, "correct");
    boolean completed = Boolean.TRUE.equals(metadata.get("completed"));

    // 4. 토픽 제목 설정
    String topicTitle;
    if ("ASSIST_WRITTEN_CATEGORY".equals(mode)) {
      topicTitle = "카테고리 기반 보조학습";
    } else if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
      topicTitle = "난이도 기반 보조학습";
    } else {
      topicTitle = "약점 보완 보조학습";
    }

    // 5. 약점 태그 계산 (필요한 경우)
    List<String> weakTags = List.of();
    if (studySession != null) {
      List<com.OhRyue.certpilot.study.domain.UserAnswer> sessionAnswers = 
          userAnswerRepository.findByUserIdAndSessionId(userId, studySession.getId()).stream()
              .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
              .toList();
      // 약점 태그 계산 로직은 필요시 추가
    }

    // 6. AI 요약 생성
    String aiSummary = aiExplanationService.summarizeWritten(
        topicTitle,
        total,
        correct,
        weakTags
    );

    // 7. XP 정보 조회 및 지급
    Integer earnedXp = null;
    Long totalXp = null;
    Integer level = null;
    Integer xpToNextLevel = null;
    Boolean leveledUp = false;
    Integer levelUpRewardPoints = 0;

    // XP 지급 (completed=true이고 xp_granted=false일 때만 지급)
    // 보조학습은 study-service에서 직접 계산한 XP를 progress-service에 직접 지급
    if (completed && studySession != null && !Boolean.TRUE.equals(studySession.getXpGranted())) {
      try {
        // 1. 보조학습 XP 정책으로 계산
        int plannedXp = calculateAssistWrittenXp(correct, total);
        
        log.info("[AssistWrittenService.summary] XP 지급 요청: sessionId={}, mode={}, total={}, correct={}, plannedXp={}, xpGranted={}",
            studySession.getId(), mode, total, correct, plannedXp, studySession.getXpGranted());
        
        if (plannedXp > 0) {
          // 2. 지급 전 지갑 정보 조회 (레벨업 여부 확인용)
          ProgressXpClient.XpWalletResponse walletBefore = progressXpClient.getWallet();
          int levelBefore = walletBefore.level();
          
          // 3. refId 생성 (세션 단위 idempotent)
          String refId = String.format("ASSIST_WRITTEN:%s:%d", userId, learningSessionId);
          
          // 4. XpReason은 ASSIST로 고정 (progress-service의 XpReason enum에 ASSIST만 있음)
          String xpReason = "ASSIST";
          
          log.info("[AssistWrittenService.summary] XP 직접 지급: delta={}, reason={}, refId={}",
              plannedXp, xpReason, refId);
          
          // 5. 직접 XP 지급 (progress-service의 /gain API 사용)
          progressXpClient.gainXp(plannedXp, xpReason, refId);
          
          // 6. 지급 후 지갑 정보 조회 (xpToNextLevel 계산을 위해)
          ProgressXpClient.XpWalletResponse walletAfter = progressXpClient.getWallet();
          
          // 7. XP 정보를 응답에 포함
          earnedXp = plannedXp; // 실제 지급된 XP (중복 호출 시 progress-service에서 0 처리)
          totalXp = walletAfter.xpTotal();
          level = walletAfter.level();
          xpToNextLevel = walletAfter.xpToNextLevel();
          
          // 8. 레벨업 여부 확인
          leveledUp = walletAfter.level() > levelBefore;
          levelUpRewardPoints = 0; // 레벨업 보상 포인트는 별도 계산 필요 (현재는 0)
          
          // 7. xp_granted 플래그 업데이트
          sessionManager.markXpGranted(studySession);
          
          log.info("[AssistWrittenService.summary] XP 지급 완료: earnedXp={}, totalXp={}, level={}, leveledUp={}",
              earnedXp, totalXp, level, leveledUp);
        } else {
          // plannedXp가 0이면 지급하지 않음
          log.info("[AssistWrittenService.summary] XP 지급 생략: plannedXp=0 (correct={}, total={})", correct, total);
          ProgressXpClient.XpWalletResponse walletResp = progressXpClient.getWallet();
          totalXp = walletResp.xpTotal();
          level = walletResp.level();
          xpToNextLevel = walletResp.xpToNextLevel();
          earnedXp = 0;
        }
      } catch (Exception e) {
        // XP 지급 실패 시 현재 지갑 정보만 조회
        log.error("Failed to grant XP in assist summary: {}", e.getMessage(), e);
        try {
          ProgressXpClient.XpWalletResponse walletResp = progressXpClient.getWallet();
          totalXp = walletResp.xpTotal();
          level = walletResp.level();
          xpToNextLevel = walletResp.xpToNextLevel();
          // earnedXp는 보조학습 XP 정책으로 계산
          earnedXp = calculateAssistWrittenXp(correct, total);
        } catch (Exception ex) {
          log.warn("Failed to retrieve XP wallet information: {}", ex.getMessage());
        }
      }
    } else {
      // XP 지급 조건 미충족 시 현재 지갑 정보만 조회
      log.warn("[AssistWrittenService.summary] XP 지급 조건 미충족: completed={}, studySession={}, xpGranted={}",
          completed, studySession != null, studySession != null ? studySession.getXpGranted() : null);
      try {
        ProgressXpClient.XpWalletResponse walletResp = progressXpClient.getWallet();
        totalXp = walletResp.xpTotal();
        level = walletResp.level();
        xpToNextLevel = walletResp.xpToNextLevel();
        // earnedXp는 보조학습 XP 정책으로 계산
        if (completed) {
          earnedXp = calculateAssistWrittenXp(correct, total);
        }
      } catch (Exception e) {
        log.warn("Failed to retrieve XP information in assist summary: {}", e.getMessage());
      }
    }

    // 8. 응답 생성
    AssistDtos.WrittenSummaryResp payload = new AssistDtos.WrittenSummaryResp(
        total,
        correct,
        aiSummary,
        completed,
        earnedXp,
        totalXp,
        level,
        xpToNextLevel,
        leveledUp,
        levelUpRewardPoints
    );

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_WRITTEN",
        "SUMMARY",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        payload,
        learningSession.getId()
    );
  }

  private Map<String, Object> parseJson(String json) {
    if (json == null || json.isBlank()) {
      return new HashMap<>();
    }
    try {
      return objectMapper.readValue(json, Map.class);
    } catch (JsonProcessingException e) {
      return new HashMap<>();
    }
  }

  private int readInt(Map<String, Object> map, String key) {
    Object value = map.get(key);
    if (value instanceof Number) {
      return ((Number) value).intValue();
    }
    return 0;
  }

  /* ================= 난이도 단건 즉시 채점 ================= */

  @Transactional
  public AssistDtos.WrittenGradeOneResp gradeOneDifficulty(Long learningSessionId, AssistDtos.WrittenGradeOneReq req) {
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

    // 2. 문제 풀기 단계 조회
    String stepCode;
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_DIFFICULTY";
    } else if ("ASSIST_WRITTEN_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_WRITTEN_WEAKNESS";
    } else {
      stepCode = "ASSIST_WRITTEN_CATEGORY";
    }
    
    LearningStep learningStep = learningSessionService.getStep(learningSession, stepCode);
    if (learningStep == null) {
      throw new IllegalStateException("학습 단계를 찾을 수 없습니다.");
    }

    // 3. StudySession 조회
    StudySession studySession = learningStep.getStudySession();
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    // 4. 문제 조회
    Question question = questionRepository.findById(req.questionId())
        .orElseThrow(() -> new IllegalStateException("문제를 찾을 수 없습니다: " + req.questionId()));

    if (question.getMode() != ExamMode.WRITTEN || question.getType() != QuestionType.MCQ) {
      throw new IllegalStateException("필기 MCQ 문제가 아닙니다.");
    }

    // 4. 채점
    String userLabel = Optional.ofNullable(req.label()).orElse("").trim();
    String correctLabel = Optional.ofNullable(question.getAnswerKey()).orElse("").trim();
    boolean isCorrect = !correctLabel.isBlank() && correctLabel.equalsIgnoreCase(userLabel);
    String explanation = Optional.ofNullable(question.getSolutionText()).orElse("");

    // 5. StudySessionItem에 답변 저장
    List<StudySessionItem> sessionItems = sessionManager.items(studySession.getId());
    StudySessionItem sessionItem = sessionItems.stream()
        .filter(item -> item.getQuestionId().equals(req.questionId()))
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
            req.questionId(),
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
                .questionId(req.questionId())
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
            learningStep.setMetadataJson(objectMapper.writeValueAsString(metadata));
            int scorePct = totalCount == 0 ? 0 : (correctCount * 100) / (int) totalCount;
            learningStep.setScorePct(scorePct);
            learningStep.setStatus("COMPLETE");
            learningStep.setUpdatedAt(Instant.now());
            learningStepRepository.save(learningStep);
            
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
        log.warn("Failed to serialize answer for question {}: {}", req.questionId(), e.getMessage());
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

    // 세션 완료 처리
    boolean allCorrect = !items.isEmpty() && wrongQuestionIds.isEmpty();
    double scorePct = items.isEmpty() ? 0.0 : (correct * 100.0) / items.size();
    sessionManager.closeSession(session, scorePct, allCorrect, Map.of(
        "total", items.size(),
        "correct", correct,
        "wrongQuestionIds", wrongQuestionIds
    ));

    // 보조학습은 각 문제 제출 시 이미 progressHookClient.submit()을 통해 XP가 지급됨
    // flowComplete는 메인학습(MICRO/REVIEW)용이므로 호출하지 않음

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

  /**
   * 보조학습 WRITTEN XP 정책 계산
   * - 정답 1개당 5 XP (단순 계산, 보너스/상한 없음)
   */
  private int calculateAssistWrittenXp(int correct, int total) {
    if (total <= 0) return 0;
    // 정답 개수 × 5 XP
    return correct * 5;
  }

  /* ================= 약점 모드 헬퍼 메서드 ================= */

  /**
   * 약점 태그 후보 선택 (최대 5개, 최소 풀이 수 3회 이상)
   */
  private List<TagAbility> pickWeakTags(String userId) {
    // abilityByTag API의 TagAbility가 중첩 record라 접근 제한이 있으므로
    // UserAnswer 기반으로 약점 태그를 추출 (accuracy와 total 정보를 직접 계산)
    List<com.OhRyue.certpilot.study.domain.UserAnswer> writtenAnswers = 
        userAnswerRepository.findByUserId(userId).stream()
            .filter(ans -> ans.getExamMode() == ExamMode.WRITTEN)
            .toList();
    
    Map<Long, List<String>> tagsByQuestion = new HashMap<>();
    writtenAnswers.stream()
        .map(com.OhRyue.certpilot.study.domain.UserAnswer::getQuestionId)
        .distinct()
        .forEach(qid -> tagsByQuestion.put(qid, questionTagRepository.findTagsByQuestionId(qid)));
    
    Map<String, int[]> tagStats = new HashMap<>();
    for (com.OhRyue.certpilot.study.domain.UserAnswer answer : writtenAnswers) {
      List<String> tags = tagsByQuestion.getOrDefault(answer.getQuestionId(), List.of());
      for (String tag : tags) {
        int[] stat = tagStats.computeIfAbsent(tag, k -> new int[2]);
        if (Boolean.TRUE.equals(answer.getCorrect())) stat[0] += 1;
        stat[1] += 1;
      }
    }
    
    return tagStats.entrySet().stream()
        .map(entry -> {
          String tag = entry.getKey();
          int correct = entry.getValue()[0];
          int total = entry.getValue()[1];
          double accuracy = total > 0 ? (correct * 100.0 / total) : 0.0;
          return new TagAbility(tag, accuracy, total);
        })
        .filter(ta -> ta.total() >= MIN_TOTAL_TRIES)
        .sorted(Comparator.comparingDouble(TagAbility::accuracy))
        .limit(MAX_WEAK_TAGS)
        .toList();
  }

  /**
   * 약점도 기반 태그별 문제 개수 배분
   */
  private Map<String, Integer> allocateCountPerTag(List<TagAbility> tags, int totalCount) {
    Map<String, Integer> alloc = new LinkedHashMap<>();
    if (tags.isEmpty() || totalCount <= 0) {
      return alloc;
    }

    // 1) 약점 점수 계산: weaknessScore = 100 - accuracy
    Map<String, Double> scoreMap = new LinkedHashMap<>();
    double sumScore = 0.0;
    for (TagAbility t : tags) {
      double score = 100.0 - t.accuracy(); // accuracy 낮을수록 score 큼
      if (score < 0) score = 0; // 방어 코드
      scoreMap.put(t.tag(), score);
      sumScore += score;
    }

    // score 합이 0인 경우(모두 100점인 극단 상황) → 균등 분배
    if (sumScore <= 0.0) {
      int base = totalCount / tags.size();
      int remain = totalCount % tags.size();
      for (int i = 0; i < tags.size(); i++) {
        TagAbility t = tags.get(i);
        int c = base + (i < remain ? 1 : 0);
        alloc.put(t.tag(), c);
      }
      return alloc;
    }

    // 2) 1차 배분: weight * totalCount
    int assigned = 0;
    for (TagAbility t : tags) {
      double weight = scoreMap.get(t.tag()) / sumScore;
      int c = (int) Math.floor(weight * totalCount); // 우선 내림
      alloc.put(t.tag(), c);
      assigned += c;
    }

    // 3) 최소 태그 다양성 보장 (상위 MIN_TAGS_IN_SET개 태그는 최소 1개)
    int tagCount = tags.size();
    int minTags = Math.min(MIN_TAGS_IN_SET, tagCount);
    for (int i = 0; i < minTags; i++) {
      TagAbility t = tags.get(i);
      String tag = t.tag();
      int current = alloc.getOrDefault(tag, 0);
      if (current == 0 && assigned < totalCount) {
        alloc.put(tag, 1);
        assigned += 1;
      }
    }

    // 4) 태그 쏠림 제한 (한 태그가 너무 많이 가져가지 않게)
    int maxPerTag = (int) Math.ceil(totalCount * MAX_SHARE_PER_TAG);
    for (TagAbility t : tags) {
      String tag = t.tag();
      int current = alloc.getOrDefault(tag, 0);
      if (current > maxPerTag) {
        int diff = current - maxPerTag;
        alloc.put(tag, maxPerTag);
        assigned -= diff;
      }
    }

    // 5) 합이 totalCount보다 작으면, 약점도가 높은 태그부터 1개씩 추가
    while (assigned < totalCount) {
      boolean changed = false;
      for (TagAbility t : tags) {
        String tag = t.tag();
        int current = alloc.getOrDefault(tag, 0);
        if (current < maxPerTag) {
          alloc.put(tag, current + 1);
          assigned += 1;
          changed = true;
          if (assigned >= totalCount) break;
        }
      }
      if (!changed) break; // 더 이상 늘릴 수 없으면 종료
    }

    // 6) 합이 totalCount보다 크면, 약점도가 낮은 태그부터 줄이기
    while (assigned > totalCount) {
      boolean changed = false;
      // 뒤에서부터(정답률 높은 태그부터) 줄여나감
      for (int i = tags.size() - 1; i >= 0; i--) {
        TagAbility t = tags.get(i);
        String tag = t.tag();
        int current = alloc.getOrDefault(tag, 0);
        if (current > 0) {
          alloc.put(tag, current - 1);
          assigned -= 1;
          changed = true;
          if (assigned <= totalCount) break;
        }
      }
      if (!changed) break;
    }

    return alloc;
  }

  /**
   * 태그별로 실제 문제 선택
   */
  private List<Question> pickQuestionsByTagAllocation(
      List<TagAbility> tags,
      Map<String, Integer> alloc,
      Map<String, List<Question>> questionsByTag,
      int totalCount
  ) {
    List<Question> selected = new ArrayList<>();
    Set<Long> selectedQuestionIds = new HashSet<>();
    int remaining = totalCount;

    // 1차 루프: 태그별 목표 개수만큼 WRITTEN+MCQ에서 뽑기
    for (TagAbility t : tags) {
      String tag = t.tag();
      int need = Math.min(alloc.getOrDefault(tag, 0), remaining);
      if (need <= 0) continue;

      List<Question> tagQuestions = questionsByTag.getOrDefault(tag, List.of());
      int toSelect = Math.min(need, tagQuestions.size());
      
      for (int i = 0; i < toSelect && selected.size() < totalCount; i++) {
        Question q = tagQuestions.get(i);
        if (!selectedQuestionIds.contains(q.getId())) {
          selected.add(q);
          selectedQuestionIds.add(q.getId());
          remaining--;
        }
      }
    }

    // 2) 여전히 remaining > 0이면, 다른 약점 태그에서 더 뽑기
    if (remaining > 0) {
      for (TagAbility t : tags) {
        if (remaining <= 0) break;
        String tag = t.tag();
        List<Question> tagQuestions = questionsByTag.getOrDefault(tag, List.of());
        
        for (Question q : tagQuestions) {
          if (remaining <= 0) break;
          if (!selectedQuestionIds.contains(q.getId())) {
            selected.add(q);
            selectedQuestionIds.add(q.getId());
            remaining--;
          }
        }
      }
    }

    return selected;
  }

  /**
   * 태그 다양성 보정 (한 태그만 도배되는 것 방지)
   */
  private List<Question> ensureTagDiversity(
      List<Question> selected,
      List<TagAbility> weakTags,
      int totalCount
  ) {
    if (selected.isEmpty()) return selected;
    if (weakTags.size() < 2) return selected; // 태그 자체가 1개면 어쩔 수 없음
    if (totalCount < 3) return selected; // 세트가 너무 작으면 강제 섞기 비효율

    // 1) 현재 세트의 태그 분포 확인
    Map<String, Long> tagCounts = new HashMap<>();
    for (Question q : selected) {
      List<String> tags = questionTagRepository.findTagsByQuestionId(q.getId());
      for (String tag : tags) {
        if (weakTags.stream().anyMatch(ta -> ta.tag().equals(tag))) {
          tagCounts.merge(tag, 1L, Long::sum);
          break; // 첫 번째 약점 태그만 카운트
        }
      }
    }

    long distinctCount = tagCounts.keySet().size();
    if (distinctCount >= 2) {
      // 이미 최소 2개 이상 태그가 섞여 있으면 OK
      return selected;
    }

    // 여기까지 왔다는 건: dominantTag 1개만 나온 상황
    // → 두 번째 약한 태그에서 강제로 문제를 섞어 넣는 시도

    // 두 번째 약한 태그 찾기
    if (weakTags.size() < 2) return selected;
    TagAbility secondWeak = weakTags.get(1);
    String secondTag = secondWeak.tag();

    // dominantTag 문제들 중 일부를 빼고, secondTag 문제로 교체 시도
    String dominantTag = tagCounts.entrySet().stream()
        .max(Map.Entry.comparingByValue())
        .map(Map.Entry::getKey)
        .orElse(null);
    
    if (dominantTag == null) return selected;

    int replaceCount = Math.min(2, selected.size() / 2); // 최대 2개 정도 교체
    List<Question> toRemove = selected.stream()
        .filter(q -> {
          List<String> qTags = questionTagRepository.findTagsByQuestionId(q.getId());
          return qTags.contains(dominantTag);
        })
        .limit(replaceCount)
        .toList();

    if (toRemove.isEmpty()) return selected;

    // secondTag에서 신규 문제 뽑기 (이미 뽑힌 questionId는 제외)
    List<Long> alreadyIds = selected.stream()
        .map(Question::getId)
        .toList();

    List<Long> secondTagQuestionIds = questionTagRepository.findQuestionIdsByTag(secondTag);
    List<Question> newQs = questionRepository.findByIdIn(secondTagQuestionIds).stream()
        .filter(q -> q.getMode() == ExamMode.WRITTEN)
        .filter(q -> q.getType() == QuestionType.MCQ)
        .filter(q -> !alreadyIds.contains(q.getId()))
        .limit(toRemove.size())
        .toList();

    if (newQs.isEmpty()) return selected; // 교체 실패 시 그대로 반환

    // 실제 교체 수행
    List<Question> result = new ArrayList<>(selected);
    result.removeAll(toRemove);
    result.addAll(newQs);

    return result;
  }
}
