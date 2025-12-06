package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.client.ProgressQueryClient;
import com.OhRyue.certpilot.study.client.ProgressXpClient;
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
  private final com.OhRyue.certpilot.study.repository.QuestionTagRepository questionTagRepository;
  private final com.OhRyue.certpilot.study.client.ProgressHookClient progressHookClient;
  private final ObjectMapper objectMapper;
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
      Set<Long> children = topicTreeService.childrenOf(topicId, "PRACTICAL");
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

    log.debug("[assist/practical/category] originalTopicIds={}, finalTopicIds={}, topicToChildMap={}",
        topicIds, finalTopicIds, topicToChildMap);

    // 2. 문제 풀 생성 (최종 토픽 ID 집합으로 조회)
    List<Question> pool = new ArrayList<>();
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(finalTopicIds, ExamMode.PRACTICAL, QuestionType.SHORT));
    pool.addAll(questionRepository
        .findByTopicIdInAndModeAndType(finalTopicIds, ExamMode.PRACTICAL, QuestionType.LONG));

    pool = pool.stream()
        .distinct()
        .sorted(Comparator.comparingLong(Question::getId))
        .toList();

    log.debug("[assist/practical/category] poolSize={}, count={}", pool.size(), want);

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
    
    log.debug("[assist/practical/category] topicQuotas={}, want={}, topicCount={}, topicIdsList={}", 
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

    log.debug("[assist/practical/category] selectedQuestions={}, count={}", 
        selectedQuestions.size(), selectedQuestions.size());

    // 3. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_PRACTICAL_CATEGORY)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_PRACTICAL_CATEGORY")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 4. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 4-1. 문제 풀기 단계
    LearningStep categoryStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_PRACTICAL_CATEGORY")
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
        .mode("ASSIST_PRACTICAL_CATEGORY")
        .examMode(ExamMode.PRACTICAL)
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
        "ASSIST_PRACTICAL_CATEGORY",
        "IN_PROGRESS",
        null,
        fetchStats(userId),
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
    if (!"ASSIST_PRACTICAL_CATEGORY".equals(learningSession.getMode())) {
      throw new IllegalStateException("카테고리 기반 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep categoryStep = learningSessionService.getStep(learningSession, "ASSIST_PRACTICAL_CATEGORY");
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
    String status = categoryStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_CATEGORY",
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

  @Transactional
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(Integer count) {
    String userId = AuthUserUtil.getCurrentUserId();
    int want = sanitizeCount(count);

    // 1. 약점 태그 추출 (report_tag_skill 테이블 기반)
    List<String> weaknessTags = new ArrayList<>();
    try {
      ProgressQueryClient.TagAbilityResp tagAbilityResp = 
          progressQueryClient.abilityByTag("PRACTICAL", 20);
      if (tagAbilityResp != null && tagAbilityResp.weaknessTags() != null) {
        weaknessTags = tagAbilityResp.weaknessTags();
      }
      log.debug("[assist/practical/weakness] userId={}, weaknessTags={} (from report_tag_skill)", 
          userId, weaknessTags);
    } catch (Exception e) {
      log.warn("[assist/practical/weakness] Failed to fetch weakness tags from progress-service: {}", 
          e.getMessage(), e);
      // Fallback: UserAnswer 기반으로 약점 태그 추출
      List<com.OhRyue.certpilot.study.domain.UserAnswer> practicalAnswers = 
          userAnswerRepository.findByUserId(userId).stream()
              .filter(ans -> ans.getExamMode() == ExamMode.PRACTICAL)
              .toList();
      
      Map<Long, List<String>> tagsByQuestion = new HashMap<>();
      practicalAnswers.stream()
          .map(com.OhRyue.certpilot.study.domain.UserAnswer::getQuestionId)
          .distinct()
          .forEach(qid -> tagsByQuestion.put(qid, questionTagRepository.findTagsByQuestionId(qid)));
      
      Map<String, int[]> tagStats = new HashMap<>();
      for (com.OhRyue.certpilot.study.domain.UserAnswer answer : practicalAnswers) {
        List<String> tags = tagsByQuestion.getOrDefault(answer.getQuestionId(), List.of());
        for (String tag : tags) {
          int[] stat = tagStats.computeIfAbsent(tag, k -> new int[2]);
          if (Boolean.TRUE.equals(answer.getCorrect())) stat[0] += 1;
          stat[1] += 1;
        }
      }
      
      weaknessTags = tagStats.entrySet().stream()
          .map(entry -> {
            String tag = entry.getKey();
            int correct = entry.getValue()[0];
            int total = entry.getValue()[1];
            double accuracy = total > 0 ? (correct * 100.0 / total) : 0.0;
            return Map.entry(tag, Map.entry(accuracy, total));
          })
          .filter(entry -> entry.getValue().getKey() < 70.0 && entry.getValue().getValue() >= 3)
          .sorted(Comparator.comparingDouble(entry -> entry.getValue().getKey()))
          .limit(5)
          .map(Map.Entry::getKey)
          .toList();
      
      log.debug("[assist/practical/weakness] userId={}, weaknessTags={} (fallback from UserAnswer)", 
          userId, weaknessTags);
    }

    // 2. 문제 풀 생성
    List<Question> pool;
    if (weaknessTags.isEmpty()) {
      // 약점 태그가 없을 때는 모든 난이도에서 문제를 찾음
      pool = new ArrayList<>();
      for (Difficulty diff : Difficulty.values()) {
        pool.addAll(questionRepository.findByModeAndTypeAndDifficulty(
            ExamMode.PRACTICAL, QuestionType.SHORT, diff));
        pool.addAll(questionRepository.findByModeAndTypeAndDifficulty(
            ExamMode.PRACTICAL, QuestionType.LONG, diff));
      }
      pool = pool.stream()
          .distinct()
          .sorted(Comparator.comparingLong(Question::getId))
          .toList();
    } else {
      // 약점 태그별로 문제를 그룹화 (카테고리 모드처럼 균등 분배)
      Map<String, List<Question>> questionsByTag = new HashMap<>();
      
      for (String tag : weaknessTags) {
        List<Long> questionIds = questionTagRepository.findQuestionIdsByTag(tag);
        List<Question> tagQuestions = questionRepository.findByIdIn(questionIds).stream()
            .filter(q -> q.getMode() == ExamMode.PRACTICAL)
            .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
            .distinct()
            .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
        questionsByTag.put(tag, tagQuestions);
      }
      
      log.debug("[assist/practical/weakness] weaknessTags={}, questionsByTag sizes={}", 
          weaknessTags, questionsByTag.entrySet().stream()
              .collect(java.util.stream.Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size())));
      
      // 각 태그의 문제를 섞기
      questionsByTag.values().forEach(Collections::shuffle);
      
      // 태그별 할당량 계산 (균등 분배)
      int tagCount = weaknessTags.size();
      int basePerTag = want / tagCount;
      int remainder = want % tagCount;
      
      Map<String, Integer> tagQuotas = new HashMap<>();
      for (int i = 0; i < weaknessTags.size(); i++) {
        String tag = weaknessTags.get(i);
        int quota = basePerTag + (i < remainder ? 1 : 0);
        tagQuotas.put(tag, quota);
      }
      
      log.debug("[assist/practical/weakness] tagQuotas={}, want={}", tagQuotas, want);
      
      // 각 태그에서 할당량만큼 선택
      List<Question> selectedQuestions = new ArrayList<>();
      for (String tag : weaknessTags) {
        List<Question> tagQuestions = questionsByTag.getOrDefault(tag, List.of());
        int quota = tagQuotas.getOrDefault(tag, 0);
        int toSelect = Math.min(quota, tagQuestions.size());
        
        for (int i = 0; i < toSelect && selectedQuestions.size() < want; i++) {
          Question q = tagQuestions.get(i);
          if (!selectedQuestions.contains(q)) {
            selectedQuestions.add(q);
          }
        }
      }
      
      // 할당량으로 부족한 경우, 문제가 많은 태그에서 추가 선택
      if (selectedQuestions.size() < want) {
        int remaining = want - selectedQuestions.size();
        
        // 태그별 선택된 문제 수 계산
        Map<String, Integer> selectedCount = new HashMap<>();
        for (Question q : selectedQuestions) {
          List<String> qTags = questionTagRepository.findTagsByQuestionId(q.getId());
          for (String tag : qTags) {
            if (weaknessTags.contains(tag)) {
              selectedCount.put(tag, selectedCount.getOrDefault(tag, 0) + 1);
              break; // 첫 번째 약점 태그만 카운트
            }
          }
        }
        
        // 문제가 많은 태그부터 추가 선택
        List<Map.Entry<String, List<Question>>> sortedTags = questionsByTag.entrySet().stream()
            .sorted((e1, e2) -> {
              int size1 = e1.getValue().size();
              int size2 = e2.getValue().size();
              int selected1 = selectedCount.getOrDefault(e1.getKey(), 0);
              int selected2 = selectedCount.getOrDefault(e2.getKey(), 0);
              // 남은 문제 수가 많고, 선택 비율이 낮은 태그 우선
              int ratio1 = size1 > 0 ? (selected1 * 100) / size1 : 0;
              int ratio2 = size2 > 0 ? (selected2 * 100) / size2 : 0;
              if (ratio1 != ratio2) {
                return Integer.compare(ratio1, ratio2); // 낮은 비율 우선
              }
              return Integer.compare(size2 - selected2, size1 - selected1); // 남은 문제가 많은 순
            })
            .toList();
        
        for (Map.Entry<String, List<Question>> entry : sortedTags) {
          if (remaining <= 0) break;
          
          String tag = entry.getKey();
          List<Question> tagQuestions = entry.getValue();
          
          for (Question q : tagQuestions) {
            if (remaining <= 0) break;
            if (!selectedQuestions.contains(q)) {
              selectedQuestions.add(q);
              remaining--;
            }
          }
        }
      }
      
      pool = selectedQuestions;
    }

    // 최종 검증: PRACTICAL 모드 문제만 포함되도록 필터링
    pool = pool.stream()
        .filter(q -> q.getMode() == ExamMode.PRACTICAL)
        .filter(q -> q.getType() == QuestionType.SHORT || q.getType() == QuestionType.LONG)
        .distinct()
        .collect(java.util.stream.Collectors.toCollection(ArrayList::new));

    log.debug("[assist/practical/weakness] userId={}, weaknessTags={}, poolSize={}, count={}",
        userId, weaknessTags, pool.size(), want);

    if (pool.isEmpty()) {
      throw new IllegalStateException("약점 보완 문제가 없습니다.");
    }

    // 3. 문제 선택 (이미 태그별로 균등하게 선택되었으므로 그대로 사용)
    // pool이 이미 selectedQuestions이므로 추가 선택 불필요

    // 4. LearningSession 생성 (topicId는 0 사용, mode는 ASSIST_PRACTICAL_WEAKNESS)
    LearningSession learningSession = learningSessionRepository.save(LearningSession.builder()
        .userId(userId)
        .topicId(ASSIST_TOPIC_ID)
        .mode("ASSIST_PRACTICAL_WEAKNESS")
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 5. 여러 LearningStep 생성 (문제 풀기 -> 오답 -> 결과)
    // 5-1. 문제 풀기 단계
    LearningStep weaknessStep = learningStepRepository.save(LearningStep.builder()
        .learningSession(learningSession)
        .stepCode("ASSIST_PRACTICAL_WEAKNESS")
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
    scopeMap.put("weaknessTags", weaknessTags);
    String scopeJson;
    try {
      scopeJson = objectMapper.writeValueAsString(scopeMap);
    } catch (JsonProcessingException e) {
      scopeJson = "{}";
    }

    StudySession studySession = StudySession.builder()
        .userId(userId)
        .mode("ASSIST_PRACTICAL_WEAKNESS")
        .examMode(ExamMode.PRACTICAL)
        .topicScopeJson(scopeJson)
        .questionCount(pool.size())
        .status("OPEN")
        .startedAt(Instant.now())
        .learningStep(weaknessStep)
        .build();

    studySession = studySessionRepository.save(studySession);

    // LearningStep에 연결
    weaknessStep.setStudySession(studySession);

    // 7. 문제 할당
    List<Long> questionIds = pool.stream()
        .map(Question::getId)
        .toList();
    sessionManager.allocateQuestions(studySession, questionIds);

    // 8. 문제 반환
    List<AssistDtos.QuizQ> items = pool.stream()
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
        "ASSIST_PRACTICAL_WEAKNESS",
        "IN_PROGRESS",
        null,
        fetchStats(userId),
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
    if (!"ASSIST_PRACTICAL_WEAKNESS".equals(learningSession.getMode())) {
      throw new IllegalStateException("약점 보완 보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    LearningStep weaknessStep = learningSessionService.getStep(learningSession, "ASSIST_PRACTICAL_WEAKNESS");
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
    String status = weaknessStep.getStatus();
    boolean completed = "COMPLETE".equals(status);
    if ("READY".equals(status)) {
      status = "IN_PROGRESS";
    }

    return new FlowDtos.StepEnvelope<>(
        studySession.getId(),
        "ASSIST_PRACTICAL",
        "ASSIST_PRACTICAL_WEAKNESS",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        sessionManager.loadMeta(studySession),
        set,
        learningSession.getId()
    );
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
    String mode = learningSession.getMode();
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(mode) && !"ASSIST_PRACTICAL_WEAKNESS".equals(mode) && !"ASSIST_PRACTICAL_CATEGORY".equals(mode)) {
      throw new IllegalStateException("보조학습 세션이 아닙니다.");
    }

    // 2. LearningStep 조회 (문제 풀기 단계)
    String stepCode;
    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_DIFFICULTY";
    } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_WEAKNESS";
    } else {
      stepCode = "ASSIST_PRACTICAL_CATEGORY";
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

          // UserAnswer 저장
          com.OhRyue.certpilot.study.domain.UserAnswer userAnswer =
              com.OhRyue.certpilot.study.domain.UserAnswer.builder()
                  .userId(userId)
                  .questionId(ans.questionId())
                  .examMode(ExamMode.PRACTICAL)
                  .questionType(q.getType())
                  .userAnswerJson(answerJson)
                  .correct(correct)
                  .score(correct ? 100 : 0)
                  .source("ASSIST_PRACTICAL")
                  .sessionId(studySession.getId())
                  .sessionItemId(sessionItem.getId())
                  .build();
          userAnswerRepository.save(userAnswer);
        } catch (JsonProcessingException e) {
          log.warn("Failed to serialize answer for question {}: {}", ans.questionId(), e.getMessage());
        }
      }

      // Progress hook (각 문제 제출 시 XP 지급: 정답 5 XP, 오답 0 XP)
      try {
        progressHookClient.submit(
            new com.OhRyue.certpilot.study.client.ProgressHookClient.SubmitPayload(
                userId,
                ExamMode.PRACTICAL.name(),
                q.getType().name(),
                correct,
                correct ? 100 : 0,
                List.of(),
                "ASSIST_PRACTICAL"
            )
        );
      } catch (Exception e) {
        log.debug("Progress hook failed: {}", e.getMessage());
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

  /* ================= 보조학습 요약 ================= */

  @Transactional(readOnly = true)
  public FlowDtos.StepEnvelope<AssistDtos.PracticalSummaryResp> summary(Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    String mode = learningSession.getMode();
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(mode) && !"ASSIST_PRACTICAL_WEAKNESS".equals(mode) && !"ASSIST_PRACTICAL_CATEGORY".equals(mode)) {
      throw new IllegalStateException("보조학습 세션이 아닙니다.");
    }

    // 2. 문제 풀기 단계 조회
    String stepCode;
    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_DIFFICULTY";
    } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_WEAKNESS";
    } else {
      stepCode = "ASSIST_PRACTICAL_CATEGORY";
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
    if ("ASSIST_PRACTICAL_CATEGORY".equals(mode)) {
      topicTitle = "카테고리 기반 보조학습";
    } else if ("ASSIST_PRACTICAL_DIFFICULTY".equals(mode)) {
      topicTitle = "난이도 기반 보조학습";
    } else {
      topicTitle = "약점 보완 보조학습";
    }

    // 5. 오답 문제 ID 목록 (실기 summary에는 mistakes가 필요)
    List<Long> wrongQuestionIds = new ArrayList<>();
    Object wrongIdsObj = metadata.get("wrongQuestionIds");
    if (wrongIdsObj instanceof List<?> wrongIds) {
      for (Object id : wrongIds) {
        if (id instanceof Long) {
          wrongQuestionIds.add((Long) id);
        } else if (id instanceof Integer) {
          wrongQuestionIds.add(((Integer) id).longValue());
        } else if (id instanceof Number) {
          wrongQuestionIds.add(((Number) id).longValue());
        }
      }
    }
    List<String> mistakes = wrongQuestionIds.stream()
        .map(String::valueOf)
        .toList();

    // 6. AI 요약 생성
    String aiSummary = aiExplanationService.summarizePractical(
        topicTitle,
        total,
        correct,
        mistakes
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
        int plannedXp = calculateAssistPracticalXp(correct, total);
        
        log.info("[AssistPracticalService.summary] XP 지급 요청: sessionId={}, mode={}, total={}, correct={}, plannedXp={}, xpGranted={}",
            studySession.getId(), mode, total, correct, plannedXp, studySession.getXpGranted());
        
        if (plannedXp > 0) {
          // 2. 지급 전 지갑 정보 조회 (레벨업 여부 확인용)
          ProgressXpClient.XpWalletResponse walletBefore = progressXpClient.getWallet();
          int levelBefore = walletBefore.level();
          
          // 3. refId 생성 (세션 단위 idempotent)
          String refId = String.format("ASSIST_PRACTICAL:%s:%d", userId, learningSessionId);
          
          // 4. XpReason은 ASSIST로 고정
          String xpReason = "ASSIST";
          
          log.info("[AssistPracticalService.summary] XP 직접 지급: delta={}, reason={}, refId={}",
              plannedXp, xpReason, refId);
          
          // 5. 직접 XP 지급 (progress-service의 /gain API 사용)
          progressXpClient.gainXp(plannedXp, xpReason, refId);
          
          // 6. 지급 후 지갑 정보 조회
          ProgressXpClient.XpWalletResponse walletAfter = progressXpClient.getWallet();
          
          // 7. XP 정보를 응답에 포함
          earnedXp = plannedXp;
          totalXp = walletAfter.xpTotal();
          level = walletAfter.level();
          xpToNextLevel = walletAfter.xpToNextLevel();
          
          // 8. 레벨업 여부 확인
          leveledUp = walletAfter.level() > levelBefore;
          levelUpRewardPoints = 0;
          
          // 9. xp_granted 플래그 업데이트
          sessionManager.markXpGranted(studySession);
          
          log.info("[AssistPracticalService.summary] XP 지급 완료: earnedXp={}, totalXp={}, level={}, leveledUp={}",
              earnedXp, totalXp, level, leveledUp);
        } else {
          // plannedXp가 0이면 지급하지 않음
          log.info("[AssistPracticalService.summary] XP 지급 생략: plannedXp=0 (correct={}, total={})", correct, total);
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
          earnedXp = calculateAssistPracticalXp(correct, total);
        } catch (Exception ex) {
          log.warn("Failed to retrieve XP wallet information: {}", ex.getMessage());
        }
      }
    } else {
      // XP 지급 조건 미충족 시 현재 지갑 정보만 조회
      log.warn("[AssistPracticalService.summary] XP 지급 조건 미충족: completed={}, studySession={}, xpGranted={}",
          completed, studySession != null, studySession != null ? studySession.getXpGranted() : null);
      try {
        ProgressXpClient.XpWalletResponse walletResp = progressXpClient.getWallet();
        totalXp = walletResp.xpTotal();
        level = walletResp.level();
        xpToNextLevel = walletResp.xpToNextLevel();
        // earnedXp는 보조학습 XP 정책으로 계산
        if (completed) {
          earnedXp = calculateAssistPracticalXp(correct, total);
        }
      } catch (Exception e) {
        log.warn("Failed to retrieve XP information in assist summary: {}", e.getMessage());
      }
    }

    // 8. 응답 생성
    AssistDtos.PracticalSummaryResp payload = new AssistDtos.PracticalSummaryResp(
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
        "ASSIST_PRACTICAL",
        "SUMMARY",
        completed ? "COMPLETE" : "IN_PROGRESS",
        null,
        fetchStats(userId),
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
  public AssistDtos.PracticalGradeOneResp gradeOneDifficulty(Long learningSessionId, AssistDtos.PracticalGradeOneReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    // 1. LearningSession 조회 및 소유자 확인
    LearningSession learningSession = learningSessionService.getLearningSession(learningSessionId);
    if (!learningSession.getUserId().equals(userId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }
    
    String mode = learningSession.getMode();
    if (!"ASSIST_PRACTICAL_DIFFICULTY".equals(mode) && !"ASSIST_PRACTICAL_WEAKNESS".equals(mode) && !"ASSIST_PRACTICAL_CATEGORY".equals(mode)) {
      throw new IllegalStateException("보조학습 세션이 아닙니다.");
    }

    // 2. 문제 풀기 단계 조회
    String stepCode;
    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_DIFFICULTY";
    } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(mode)) {
      stepCode = "ASSIST_PRACTICAL_WEAKNESS";
    } else {
      stepCode = "ASSIST_PRACTICAL_CATEGORY";
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

    if (question.getMode() != ExamMode.PRACTICAL) {
      throw new IllegalStateException("실기 문제가 아닙니다.");
    }

    // 4. AI 채점
    AIExplanationService.PracticalResult result =
        aiExplanationService.explainAndScorePractical(question, req.userText());

    boolean correct = Optional.ofNullable(result.correct()).orElse(false);
    String baseExplain = Optional.ofNullable(question.getSolutionText()).orElse("");
    String aiExplain = result.explain();

    // 5. StudySessionItem에 답변 저장
    List<StudySessionItem> sessionItems = sessionManager.items(studySession.getId());
    StudySessionItem sessionItem = sessionItems.stream()
        .filter(item -> item.getQuestionId().equals(req.questionId()))
        .findFirst()
        .orElse(null);

    if (sessionItem != null) {
      try {
        Map<String, Object> answerPayload = new HashMap<>();
        answerPayload.put("answer", req.userText());
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
            req.questionId(),
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
                .questionId(req.questionId())
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
            learningStep.setMetadataJson(objectMapper.writeValueAsString(metadata));
            int scorePct = totalCount == 0 ? 0 : (correctCount * 100) / (int) totalCount;
            learningStep.setScorePct(scorePct);
            learningStep.setStatus("COMPLETE");
            learningStep.setUpdatedAt(Instant.now());
            learningStepRepository.save(learningStep);
            
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
        log.warn("Failed to serialize answer for question {}: {}", req.questionId(), e.getMessage());
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

  /**
   * 보조학습 PRACTICAL XP 정책 계산
   * - 정답 1개당 5 XP (단순 계산, 보너스/상한 없음)
   */
  private int calculateAssistPracticalXp(int correct, int total) {
    if (total <= 0) return 0;
    // 정답 개수 × 5 XP
    return correct * 5;
  }
}
