package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
import com.OhRyue.certpilot.study.domain.Question;
import com.OhRyue.certpilot.study.domain.StudySession;
import com.OhRyue.certpilot.study.domain.StudySessionItem;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import com.OhRyue.certpilot.study.repository.QuestionRepository;
import com.OhRyue.certpilot.study.repository.StudySessionItemRepository;
import com.OhRyue.certpilot.study.repository.StudySessionRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class LearningSessionService {

  private final LearningSessionRepository sessionRepo;
  private final LearningStepRepository stepRepo;
  private final StudySessionManager sessionManager;
  private final ObjectMapper objectMapper;
  private final TopicTreeService topicTreeService;
  private final QuestionRepository questionRepository;
  private final StudySessionRepository sessionRepository;
  private final StudySessionItemRepository studySessionItemRepository;

  /** 필기(WRITTEN) 단계 순서 */
  private static final List<String> ORDER_WRITTEN = List.of(
      "CONCEPT", "MINI", "MCQ", "REVIEW_WRONG", "SUMMARY"
  );
  /** 실기(PRACTICAL) 단계 순서 */
  private static final List<String> ORDER_PRACTICAL = List.of(
      "CONCEPT", "MINI", "SHORT", "REVIEW_WRONG", "SUMMARY"
  );
  /** 리뷰(REVIEW) 단계 순서 - 필기 */
  private static final List<String> ORDER_REVIEW_WRITTEN = List.of(
      "MCQ", "REVIEW_WRONG", "SUMMARY"
  );
  /** 리뷰(REVIEW) 단계 순서 - 실기 */
  private static final List<String> ORDER_REVIEW_PRACTICAL = List.of(
      "SHORT", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_WRITTEN_DIFFICULTY = List.of(
      "ASSIST_WRITTEN_DIFFICULTY", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_PRACTICAL_DIFFICULTY = List.of(
      "ASSIST_PRACTICAL_DIFFICULTY", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_WRITTEN_WEAKNESS = List.of(
      "ASSIST_WRITTEN_WEAKNESS", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_WRITTEN_CATEGORY = List.of(
      "ASSIST_WRITTEN_CATEGORY", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_PRACTICAL_CATEGORY = List.of(
      "ASSIST_PRACTICAL_CATEGORY", "REVIEW_WRONG", "SUMMARY"
  );

  private static final List<String> ORDER_ASSIST_PRACTICAL_WEAKNESS = List.of(
      "ASSIST_PRACTICAL_WEAKNESS", "REVIEW_WRONG", "SUMMARY"
  );

  /** 모드에 따른 단계 순서 (ExamMode 버전) */
  private List<String> orderOf(ExamMode mode) {
    return (mode == ExamMode.PRACTICAL) ? ORDER_PRACTICAL : ORDER_WRITTEN;
  }

  /** 모드에 따른 단계 순서 (String 버전 - 오버로드) */
  private List<String> orderOf(String modeStr) {
    // REVIEW 모드는 특별 처리 (기본값은 필기 Review)
    if ("REVIEW".equalsIgnoreCase(modeStr)) {
      return ORDER_REVIEW_WRITTEN;
    }
    // MICRO 모드는 examMode 정보가 없으면 판단할 수 없으므로 parseMode로 처리
    // (실제로는 findNextStep에서 단계를 보고 판단함)
    // difficulty 기반 보조학습 모드 처리
    if ("ASSIST_WRITTEN_DIFFICULTY".equals(modeStr)) {
      return ORDER_ASSIST_WRITTEN_DIFFICULTY;
    }
    if ("ASSIST_PRACTICAL_DIFFICULTY".equals(modeStr)) {
      return ORDER_ASSIST_PRACTICAL_DIFFICULTY;
    }
    // weakness 기반 보조학습 모드 처리
    if ("ASSIST_WRITTEN_WEAKNESS".equals(modeStr)) {
      return ORDER_ASSIST_WRITTEN_WEAKNESS;
    }
    // category 기반 보조학습 모드 처리
    if ("ASSIST_WRITTEN_CATEGORY".equals(modeStr)) {
      return ORDER_ASSIST_WRITTEN_CATEGORY;
    }
    if ("ASSIST_PRACTICAL_CATEGORY".equals(modeStr)) {
      return ORDER_ASSIST_PRACTICAL_CATEGORY;
    }
    if ("ASSIST_PRACTICAL_WEAKNESS".equals(modeStr)) {
      return ORDER_ASSIST_PRACTICAL_WEAKNESS;
    }
    return orderOf(parseMode(modeStr));
  }

  /** 문자열 → ExamMode (기본 WRITTEN) */
  private ExamMode parseMode(String modeStr) {
    if (modeStr == null || modeStr.isBlank()) return ExamMode.WRITTEN;
    try {
      return ExamMode.valueOf(modeStr.trim().toUpperCase());
    } catch (IllegalArgumentException ex) {
      return ExamMode.WRITTEN;
    }
  }

  /**
   * 학습 세션 시작 (resume=true면 최신 세션 재개)
   * - 외부 시그니처에서 userId 제거, 내부에서 JWT 기반 AuthUserUtil 사용
   * - resume=true: IN_PROGRESS 상태인 세션만 재사용
   * - resume=false: 기존 IN_PROGRESS 세션이 있으면 CLOSED 처리하고 DONE으로 변경 후 새로 생성
   */
  @Transactional
  public StartResp start(StartReq req) {
    String userId = AuthUserUtil.getCurrentUserId();
    String modeStr = String.valueOf(req.mode());

    if (Boolean.TRUE.equals(req.resume())) {
      // IN_PROGRESS 상태인 세션만 재사용
      var found = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(
          userId, req.topicId(), modeStr);
      if (found.isPresent() && "IN_PROGRESS".equals(found.get().getStatus())) {
        var s = found.get();
        s.setUpdatedAt(Instant.now());
        return new StartResp(s.getId(), s.getStatus());
      }
    } else {
      // resume=false: 기존 IN_PROGRESS 세션이 있으면 CLOSED 처리하고 DONE으로 변경
      var existingInProgress = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(
          userId, req.topicId(), modeStr);
      if (existingInProgress.isPresent() && "IN_PROGRESS".equals(existingInProgress.get().getStatus())) {
        var oldSession = existingInProgress.get();
        // 모든 StudySession을 CLOSED 처리
        sessionManager.closeAllSessionsForLearningSession(oldSession);
        // LearningSession의 status를 DONE으로 변경
        oldSession.setStatus("DONE");
        oldSession.setUpdatedAt(Instant.now());
        sessionRepo.save(oldSession);
      }
    }

    // 어떤 타입(ExamMode/String) 이든 문자열로 저장

    var s = sessionRepo.save(LearningSession.builder()
        .userId(userId)
        .topicId(req.topicId())
        .mode(modeStr)
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // examMode 파싱: req.examMode()를 우선 사용, 없으면 modeStr에서 파싱
    ExamMode examMode;
    if (req.examMode() != null && !req.examMode().isBlank()) {
      try {
        examMode = ExamMode.valueOf(req.examMode().trim().toUpperCase());
      } catch (IllegalArgumentException ex) {
        // 파싱 실패 시 modeStr에서 파싱 시도
        examMode = parseMode(modeStr);
      }
    } else {
      // req.examMode()가 없으면 modeStr에서 파싱
      // MICRO 모드의 경우 req.examMode()가 필수이지만, 없으면 기본값으로 실기(PRACTICAL) 처리
      if ("MICRO".equalsIgnoreCase(modeStr)) {
        examMode = ExamMode.PRACTICAL; // 기본값: 실기 MICRO
      } else {
        examMode = parseMode(modeStr);
      }
    }
    
    // Review 모드일 경우 별도 처리
    if ("REVIEW".equals(modeStr)) {
      // Review 모드: examMode 확인 (실기 Review는 PRACTICAL, 필기 Review는 WRITTEN)
      ExamMode reviewExamMode = examMode; // 위에서 파싱한 examMode 사용
      
      // Review 모드: rootTopicId의 직접 자식 토픽들만 조회 (2레벨 → 3레벨)
      // REVIEW 모드는 2레벨 토픽에서 실행되며, 자식(3레벨) 토픽들의 문제를 사용합니다.
      String examModeStr = req.examMode() != null && !req.examMode().isBlank() 
          ? req.examMode().trim().toUpperCase() 
          : null;
      Set<Long> topicIds = topicTreeService.childrenOf(req.topicId(), examModeStr);
      
      if (topicIds.isEmpty() || topicIds.equals(Set.of(req.topicId()))) {
        // 자식 토픽을 찾지 못한 경우: cert-service API 호출 실패 또는 실제로 자식이 없음
        // 이 경우 fallback으로 rootTopicId를 사용하지 않고, 명확한 에러를 발생시킴
        throw new IllegalStateException(
            String.format("REVIEW 모드: rootTopicId=%d의 자식 토픽을 찾을 수 없습니다. " +
                         "cert-service API 확인 필요 (parentId=%d, mode=%s)", 
                         req.topicId(), req.topicId(), examModeStr));
      }
      
      // 단계 순서 결정 (실기 Review는 SHORT 단계, 필기 Review는 MCQ 단계)
      List<String> reviewOrder = (reviewExamMode == ExamMode.PRACTICAL) 
          ? ORDER_REVIEW_PRACTICAL 
          : ORDER_REVIEW_WRITTEN;
      
      // 단계 초기화 및 문제 사전 할당
      for (String stepCode : reviewOrder) {
        LearningStep step = stepRepo.save(LearningStep.builder()
            .learningSession(s)
            .stepCode(stepCode)
            .status("READY")
            .scorePct(null)
            .metadataJson(null)
            .createdAt(Instant.now())
            .updatedAt(Instant.now())
            .build());
        
        // 필기 Review: MCQ 단계의 경우 문제 사전 할당
        if ("MCQ".equals(stepCode) && reviewExamMode == ExamMode.WRITTEN) {
          sessionManager.createAndAllocateSessionForReviewStep(
              step, userId, req.topicId(), reviewExamMode, QuestionType.MCQ, 10, topicIds);
        }
        // 실기 Review: SHORT 단계의 경우 문제 사전 할당 (SHORT 10문제만 사용, LONG 제거)
        else if ("SHORT".equals(stepCode) && reviewExamMode == ExamMode.PRACTICAL) {
          try {
            // SHORT 10문제만 사용 (LONG 제거)
            List<Question> questions = questionRepository.pickRandomByTopicIn(
                topicIds, ExamMode.PRACTICAL, QuestionType.SHORT, PageRequest.of(0, 10));
            
            if (questions.isEmpty()) {
              throw new IllegalStateException("문제가 부족합니다. rootTopicId: " + req.topicId());
            }
            
            // StudySession 생성 및 문제 할당
            String scopeJson = objectMapper.writeValueAsString(Map.of("rootTopicId", req.topicId()));
            StudySession studySession = StudySession.builder()
                .userId(userId)
                .mode("REVIEW")
                .examMode(ExamMode.PRACTICAL)
                .topicScopeJson(scopeJson)
                .questionCount(10)
                .status("OPEN")
                .startedAt(Instant.now())
                .learningStep(step)
                .build();
            studySession = sessionRepository.save(studySession);
            
            // LearningStep에 연결
            step.setStudySession(studySession);
            stepRepo.save(step);
            
            // 문제 할당
            List<Long> questionIds = questions.stream().map(Question::getId).toList();
            for (int i = 0; i < questionIds.size(); i++) {
              Long questionId = questionIds.get(i);
              studySessionItemRepository.save(StudySessionItem.builder()
                  .sessionId(studySession.getId())
                  .questionId(questionId)
                  .orderNo(i + 1)
                  .userAnswerJson(null)
                  .correct(null)
                  .score(null)
                  .createdAt(Instant.now())
                  .build());
            }
          } catch (JsonProcessingException e) {
            throw new IllegalStateException("세션 생성 중 오류가 발생했습니다.", e);
          }
        }
        // REVIEW_WRONG, SUMMARY는 문제 할당 없음
      }
      
      // 첫 단계를 IN_PROGRESS로 설정
      String firstStepCode = reviewOrder.get(0);
      LearningStep firstStep = stepRepo.findByLearningSessionIdAndStepCode(s.getId(), firstStepCode)
          .orElse(null);
      if (firstStep != null) {
        firstStep.setStatus("IN_PROGRESS");
        stepRepo.save(firstStep);
      }
    } else {
      // 일반 모드 (WRITTEN, PRACTICAL)
      // examMode에 맞춰 단계 초기화 및 문제 사전 할당
      // modeStr이 "MICRO"인 경우에도 examMode로 단계를 결정해야 함
      List<String> stepOrder = orderOf(examMode);  // examMode를 사용하여 단계 순서 결정
      for (String stepCode : stepOrder) {
        LearningStep step = stepRepo.save(LearningStep.builder()
            .learningSession(s)
            .stepCode(stepCode)
            .status("READY")
            .scorePct(null)
            .metadataJson(null)
            .createdAt(Instant.now())
            .updatedAt(Instant.now())
            .build());
        
        // MINI, MCQ, SHORT 단계의 경우 문제 사전 할당
        if ("MINI".equals(stepCode)) {
          // MINI 단계는 필기/실기 모드 모두 OX 문제 사용
          sessionManager.createAndAllocateSessionForStep(
              step, userId, req.topicId(), examMode, QuestionType.OX, 4);
        } else if ("MCQ".equals(stepCode)) {
          // MCQ는 필기 모드에만 있음
          if (examMode == ExamMode.WRITTEN) {
            sessionManager.createAndAllocateSessionForStep(
                step, userId, req.topicId(), examMode, QuestionType.MCQ, 5);
          }
        } else if ("SHORT".equals(stepCode)) {
          // SHORT는 실기 모드에만 있음 (문제 할당은 나중에 SHORT_SET에서 수행)
          // 여기서는 StudySession만 생성하고, 문제 할당은 하지 않음
          // MINI 단계와 같은 StudySession을 공유하므로 별도 StudySession 생성 불필요
        }
        // CONCEPT, REVIEW_WRONG, SUMMARY 등은 문제 할당 없음
      }
      
      // CONCEPT 단계를 첫 단계로 설정
      LearningStep conceptStep = stepRepo.findByLearningSessionIdAndStepCode(s.getId(), "CONCEPT")
          .orElse(null);
      if (conceptStep != null) {
        conceptStep.setStatus("IN_PROGRESS");
        stepRepo.save(conceptStep);
      }
    }
    
    return new StartResp(s.getId(), s.getStatus());
  }

  @Transactional(readOnly = true)
  public SessionResp get(Long sessionId) {
    String currentUserId = AuthUserUtil.getCurrentUserId();

    var s = sessionRepo.findById(sessionId)
        .orElseThrow(() -> new NoSuchElementException("session not found"));

    if (!Objects.equals(s.getUserId(), currentUserId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }

    var steps = stepRepo.findByLearningSessionIdOrderByIdAsc(sessionId).stream()
        .map(x -> new SessionResp.StepItem(
            x.getId(),
            x.getStepCode(),
            x.getStatus(),
            x.getScorePct(),
            x.getMetadataJson()))
        .toList();
    
    // 현재 진행 단계 찾기 (IN_PROGRESS 또는 첫 번째 READY 단계)
    String currentStep = null;
    
    // 1. 먼저 IN_PROGRESS 상태인 단계 찾기
    for (SessionResp.StepItem step : steps) {
      if ("IN_PROGRESS".equals(step.state())) {
        currentStep = step.step();
        break;
      }
    }
    
    // 2. IN_PROGRESS가 없으면, COMPLETE된 단계 다음의 첫 번째 READY 단계 찾기
    if (currentStep == null) {
      boolean foundComplete = false;
      for (SessionResp.StepItem step : steps) {
        if ("COMPLETE".equals(step.state())) {
          foundComplete = true;
        } else if (foundComplete && "READY".equals(step.state())) {
          currentStep = step.step();
          break;
        }
      }
    }
    
    // 3. 여전히 없으면 첫 번째 READY 단계
    if (currentStep == null) {
      for (SessionResp.StepItem step : steps) {
        if ("READY".equals(step.state())) {
          currentStep = step.step();
          break;
        }
      }
    }
    
    return new SessionResp(s.getId(), s.getTopicId(), s.getMode(),
        s.getStatus(), currentStep, steps);
  }

  /**
   * LearningSession 조회 (소유자 확인 포함)
   */
  @Transactional(readOnly = true)
  public LearningSession getLearningSession(Long learningSessionId) {
    String currentUserId = AuthUserUtil.getCurrentUserId();

    var session = sessionRepo.findById(learningSessionId)
        .orElseThrow(() -> new NoSuchElementException("LearningSession not found"));

    if (!Objects.equals(session.getUserId(), currentUserId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }

    return session;
  }

  /**
   * LearningSession 조회 (생성하지 않음, 없으면 Optional.empty())
   */
  @Transactional(readOnly = true)
  public Optional<LearningSession> findLearningSession(String userId, Long topicId, ExamMode examMode) {
    String modeStr = examMode.name();
    return sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(userId, topicId, modeStr)
        .filter(s -> "IN_PROGRESS".equals(s.getStatus()));
  }

  /**
   * 현재 step을 COMPLETE 처리(프런트가 '해당 단계 문제를 전부 완료'한 뒤 호출).
   * 다음 READY 단계로 이동. 남은 READY 단계가 없으면 DONE.
   * - JWT 기반 유저 검증 추가
   * - 단계별 완료 조건 검증
   * - StudySession 종료 처리
   * - 다음 단계 자동 활성화
   */
  @Transactional
  public AdvanceResp advance(AdvanceReq req) {
    String currentUserId = AuthUserUtil.getCurrentUserId();

    var s = sessionRepo.findById(req.sessionId())
        .orElseThrow(() -> new NoSuchElementException("session not found"));

    if (!Objects.equals(s.getUserId(), currentUserId)) {
      throw new IllegalStateException("세션 소유자가 아닙니다.");
    }

    var steps = stepRepo.findByLearningSessionIdOrderByIdAsc(s.getId());
    var current = steps.stream()
        .filter(st -> st.getStepCode().equals(req.step()))
        .findFirst()
        .orElseThrow(() -> new NoSuchElementException("step not found in session"));

    // 현재 단계가 IN_PROGRESS 상태인지 확인
    if (!"IN_PROGRESS".equals(current.getStatus()) && !"READY".equals(current.getStatus())) {
      throw new IllegalStateException("단계가 진행 가능한 상태가 아닙니다. 현재 상태: " + current.getStatus());
    }

    // 단계별 완료 조건 검증 (문제가 있는 단계만)
    // Review 모드와 일반 모드 구분
    if ("REVIEW".equals(s.getMode())) {
      if (requiresCompletionCheckForReview(current.getStepCode(), s)) {
        validateStepCompletionForReview(current, s);
      }
    } else {
      if (requiresCompletionCheck(current.getStepCode())) {
        validateStepCompletion(current, s);
      }
    }

    // 단계를 COMPLETE로 변경
    current.setStatus("COMPLETE");
    current.setScorePct(req.score());
    current.setMetadataJson(req.detailsJson());
    current.setUpdatedAt(Instant.now());
    stepRepo.save(current);

    // StudySession 종료 처리 (MINI, MCQ, SHORT 단계 완료 시)
    boolean shouldCloseSession;
    if ("REVIEW".equals(s.getMode())) {
      shouldCloseSession = hasStudySessionForReview(current, s);
    } else {
      shouldCloseSession = hasStudySession(current);
    }
    
    if (shouldCloseSession) {
      com.OhRyue.certpilot.study.domain.StudySession studySession;
      
      // Review 모드의 SHORT 단계는 직접 StudySession을 가짐
      // 일반 모드의 SHORT 단계는 MINI 단계와 같은 StudySession을 공유
      if ("SHORT".equals(current.getStepCode()) && !"REVIEW".equals(s.getMode())) {
        var miniStep = stepRepo.findByLearningSessionIdAndStepCode(s.getId(), "MINI")
            .orElseThrow(() -> new IllegalStateException("MINI 단계를 찾을 수 없습니다."));
        studySession = miniStep.getStudySession();
      } else {
        studySession = current.getStudySession();
      }
      
      if (studySession != null && !"CLOSED".equals(studySession.getStatus()) && !"SUBMITTED".equals(studySession.getStatus())) {
        // 메타데이터에서 점수 추출
        Integer scorePct = req.score() != null ? req.score() : current.getScorePct();
        if (scorePct == null) scorePct = 0;
        
        sessionManager.closeSession(studySession, scorePct, parseMetadata(req.detailsJson()));
      }
    }

    // 세션의 mode(String) 기준으로 다음 단계 탐색 및 활성화
    // 오답이 없으면 REVIEW_WRONG 단계를 건너뛰고 SUMMARY로 이동
    String next = findNextStep(steps, s.getMode(), current, s);
    
    if (next != null) {
      // 하위 호환성: "SHORT" 단계가 없으면 "PRACTICAL" 단계를 찾아봄 (이전 버전의 세션)
      var nextStepOpt = steps.stream()
          .filter(x -> x.getStepCode().equals(next))
          .findFirst();
      
      // "SHORT"가 없고 "PRACTICAL"이 있으면 "PRACTICAL" 사용
      if (nextStepOpt.isEmpty() && "SHORT".equals(next)) {
        nextStepOpt = steps.stream()
            .filter(x -> x.getStepCode().equals("PRACTICAL"))
            .findFirst();
      }
      
      var nextStep = nextStepOpt.orElseThrow(() -> 
          new IllegalStateException("다음 단계를 찾을 수 없습니다: " + next));
      
      // 다음 단계를 IN_PROGRESS로 활성화
      nextStep.setStatus("IN_PROGRESS");
      nextStep.setUpdatedAt(Instant.now());
      stepRepo.save(nextStep);
    }

    if (next == null) {
      s.setStatus("DONE");
      s.setUpdatedAt(Instant.now());
      sessionRepo.save(s);
      return new AdvanceResp(s.getId(), "END", s.getStatus());
    } else {
      s.setUpdatedAt(Instant.now());
      sessionRepo.save(s);
      return new AdvanceResp(s.getId(), next, s.getStatus());
    }
  }

  /**
   * 단계별 완료 조건 검증이 필요한지 확인
   */
  private boolean requiresCompletionCheck(String stepCode) {
    return "MINI".equals(stepCode) || 
           "MCQ".equals(stepCode) || 
           "SHORT".equals(stepCode) ||
           "ASSIST_WRITTEN_DIFFICULTY".equals(stepCode) ||
           "ASSIST_PRACTICAL_DIFFICULTY".equals(stepCode) ||
           "ASSIST_WRITTEN_CATEGORY".equals(stepCode) ||
           "ASSIST_PRACTICAL_CATEGORY".equals(stepCode);
  }

  /**
   * Review 모드에서 단계별 완료 조건 검증이 필요한지 확인
   */
  private boolean requiresCompletionCheckForReview(String stepCode, LearningSession session) {
    // 필기 Review 모드: MCQ만 검증
    // 실기 Review 모드: SHORT만 검증
    // 실기 Review 모드인지 확인 (SHORT 단계가 있으면 실기 Review)
    boolean isPracticalReview = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "SHORT")
        .isPresent();
    
    if (isPracticalReview) {
      return "SHORT".equals(stepCode);
    } else {
      return "MCQ".equals(stepCode);
    }
  }

  /**
   * 단계별 완료 조건 검증
   */
  private void validateStepCompletion(LearningStep step, LearningSession session) {
    com.OhRyue.certpilot.study.domain.StudySession studySession;
    
    // SHORT 단계는 MINI 단계와 같은 StudySession을 공유하므로 MINI 단계에서 가져옴
    if ("SHORT".equals(step.getStepCode())) {
      var miniStep = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "MINI")
          .orElseThrow(() -> new IllegalStateException("MINI 단계를 찾을 수 없습니다."));
      studySession = miniStep.getStudySession();
    } else {
      studySession = step.getStudySession();
    }
    
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    var items = sessionManager.items(studySession.getId());
    if (items.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // MINI와 MCQ는 각각 별도의 StudySession을 가지므로 모든 아이템이 해당 단계의 문제
    // PRACTICAL은 MINI와 같은 StudySession을 공유하므로 orderNo > 4로 필터링
    // Review 모드의 MCQ는 별도 StudySession이므로 모든 아이템
    // ASSIST_*_DIFFICULTY는 별도 StudySession이므로 모든 아이템
    List<com.OhRyue.certpilot.study.domain.StudySessionItem> stepItems;
    if ("MINI".equals(step.getStepCode())) {
      // MINI는 orderNo 1-4
      stepItems = items.stream()
          .filter(item -> item.getOrderNo() <= 4)
          .toList();
    } else if ("MCQ".equals(step.getStepCode())) {
      // MCQ는 별도 StudySession이므로 모든 아이템 (일반 모드와 Review 모드 모두)
      stepItems = items;
    } else if ("SHORT".equals(step.getStepCode())) {
      // SHORT는 MINI와 같은 StudySession을 공유하므로 orderNo > 4
      stepItems = items.stream()
          .filter(item -> item.getOrderNo() > 4)
          .toList();
    } else if ("ASSIST_WRITTEN_DIFFICULTY".equals(step.getStepCode()) || 
               "ASSIST_WRITTEN_WEAKNESS".equals(step.getStepCode()) ||
               "ASSIST_WRITTEN_CATEGORY".equals(step.getStepCode()) ||
               "ASSIST_PRACTICAL_DIFFICULTY".equals(step.getStepCode()) ||
               "ASSIST_PRACTICAL_WEAKNESS".equals(step.getStepCode()) ||
               "ASSIST_PRACTICAL_CATEGORY".equals(step.getStepCode())) {
      // ASSIST_*는 별도 StudySession이므로 모든 아이템
      stepItems = items;
    } else {
      return; // 검증 불필요
    }

    // 모든 문제에 답변이 있는지 확인
    long answeredCount = stepItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();

    if (answeredCount < stepItems.size()) {
      throw new IllegalStateException(
          String.format("%s 단계의 모든 문제를 풀어야 합니다. (완료: %d/%d)", 
              step.getStepCode(), answeredCount, stepItems.size()));
    }
  }

  /**
   * Review 모드에서 단계별 완료 조건 검증
   */
  private void validateStepCompletionForReview(LearningStep step, LearningSession session) {
    com.OhRyue.certpilot.study.domain.StudySession studySession = step.getStudySession();
    
    if (studySession == null) {
      throw new IllegalStateException("StudySession이 초기화되지 않았습니다.");
    }

    var items = sessionManager.items(studySession.getId());
    if (items.isEmpty()) {
      throw new IllegalStateException("세션에 할당된 문제가 없습니다.");
    }

    // Review 모드에서는:
    // - 필기 Review: MCQ 단계는 별도 StudySession이므로 모든 아이템
    // - 실기 Review: SHORT 단계는 별도 StudySession이므로 모든 아이템
    List<com.OhRyue.certpilot.study.domain.StudySessionItem> stepItems = items;

    // 모든 문제에 답변이 있는지 확인
    long answeredCount = stepItems.stream()
        .filter(item -> item.getUserAnswerJson() != null && !item.getUserAnswerJson().isBlank())
        .count();

    if (answeredCount < stepItems.size()) {
      throw new IllegalStateException(
          String.format("%s 단계의 모든 문제를 풀어야 합니다. (완료: %d/%d)", 
              step.getStepCode(), answeredCount, stepItems.size()));
    }
  }

  /**
   * 단계에 StudySession이 연결되어 있는지 확인
   */
  private boolean hasStudySession(LearningStep step) {
    return "MINI".equals(step.getStepCode()) || 
           "MCQ".equals(step.getStepCode()) || 
           "SHORT".equals(step.getStepCode()) ||
           "ASSIST_WRITTEN_DIFFICULTY".equals(step.getStepCode()) ||
           "ASSIST_PRACTICAL_DIFFICULTY".equals(step.getStepCode()) ||
           "ASSIST_WRITTEN_WEAKNESS".equals(step.getStepCode()) ||
           "ASSIST_WRITTEN_CATEGORY".equals(step.getStepCode()) ||
           "ASSIST_PRACTICAL_WEAKNESS".equals(step.getStepCode()) ||
           "ASSIST_PRACTICAL_CATEGORY".equals(step.getStepCode());
  }

  /**
   * Review 모드에서 단계에 StudySession이 연결되어 있는지 확인
   */
  private boolean hasStudySessionForReview(LearningStep step, LearningSession session) {
    // 필기 Review 모드: MCQ만 StudySession 사용
    // 실기 Review 모드: SHORT만 StudySession 사용
    // 실기 Review 모드인지 확인 (SHORT 단계가 있으면 실기 Review)
    boolean isPracticalReview = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "SHORT")
        .isPresent();
    
    if (isPracticalReview) {
      return "SHORT".equals(step.getStepCode());
    } else {
      return "MCQ".equals(step.getStepCode());
    }
  }

  /**
   * 다음 단계 찾기 (오답이 없으면 REVIEW_WRONG 건너뛰기)
   */
  private String findNextStep(List<LearningStep> steps, String mode, LearningStep currentStep, LearningSession session) {
    // Review 모드인 경우 실기 Review인지 필기 Review인지 확인
    List<String> stepOrder;
    if ("REVIEW".equals(mode)) {
      // 실기 Review 모드인지 확인 (SHORT 단계가 있으면 실기 Review)
      boolean isPracticalReview = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "SHORT")
        .isPresent();
      stepOrder = isPracticalReview ? ORDER_REVIEW_PRACTICAL : ORDER_REVIEW_WRITTEN;
    } else if ("MICRO".equals(mode)) {
      // MICRO 모드: 이미 생성된 단계를 보고 필기/실기 구분
      // SHORT 단계가 있으면 실기 MICRO, MCQ 단계가 있으면 필기 MICRO
      boolean hasShort = steps.stream().anyMatch(s -> "SHORT".equals(s.getStepCode()));
      boolean hasMcq = steps.stream().anyMatch(s -> "MCQ".equals(s.getStepCode()));
      if (hasShort) {
        // 실기 MICRO
        stepOrder = ORDER_PRACTICAL;
      } else if (hasMcq) {
        // 필기 MICRO
        stepOrder = ORDER_WRITTEN;
      } else {
        // 기본값은 실기 (하위 호환성)
        stepOrder = ORDER_PRACTICAL;
      }
    } else {
      stepOrder = orderOf(mode);
    }
    
    // 현재 단계의 오답 목록 확인
    // REVIEW_WRONG 단계를 완료할 때는 이전 단계(MCQ, PRACTICAL, 또는 ASSIST_*_DIFFICULTY)의 오답을 확인해야 함
    boolean hasWrongAnswers;
    if ("REVIEW_WRONG".equals(currentStep.getStepCode())) {
      // REVIEW_WRONG 단계를 완료할 때는 이전 단계의 오답을 확인
      String previousStepCode;
      if ("REVIEW".equals(mode)) {
        // Review 모드에서는 MCQ만 있음
        previousStepCode = "MCQ";
      } else if ("ASSIST_WRITTEN_DIFFICULTY".equals(mode)) {
        previousStepCode = "ASSIST_WRITTEN_DIFFICULTY";
      } else if ("ASSIST_PRACTICAL_DIFFICULTY".equals(mode)) {
        previousStepCode = "ASSIST_PRACTICAL_DIFFICULTY";
      } else if ("ASSIST_WRITTEN_WEAKNESS".equals(mode)) {
        previousStepCode = "ASSIST_WRITTEN_WEAKNESS";
      } else if ("ASSIST_PRACTICAL_WEAKNESS".equals(mode)) {
        previousStepCode = "ASSIST_PRACTICAL_WEAKNESS";
      } else if ("ASSIST_WRITTEN_CATEGORY".equals(mode)) {
        previousStepCode = "ASSIST_WRITTEN_CATEGORY";
      } else if ("ASSIST_PRACTICAL_CATEGORY".equals(mode)) {
        previousStepCode = "ASSIST_PRACTICAL_CATEGORY";
      } else {
        // MICRO 모드도 실기 모드이므로 SHORT 단계 사용
        previousStepCode = (mode.equals("PRACTICAL") || mode.equals("MICRO")) ? "SHORT" : "MCQ";
      }
      var previousStep = steps.stream()
          .filter(x -> x.getStepCode().equals(previousStepCode))
          .findFirst()
          .orElse(null);
      hasWrongAnswers = previousStep != null && hasWrongAnswers(previousStep);
    } else {
      // 다른 단계를 완료할 때는 현재 단계의 오답을 확인
      hasWrongAnswers = hasWrongAnswers(currentStep);
    }
    
    for (String stepCode : stepOrder) {
      // 하위 호환성: "SHORT" 단계가 없으면 "PRACTICAL" 단계를 찾아봄 (이전 버전의 세션)
      var foundOpt = steps.stream()
          .filter(x -> x.getStepCode().equals(stepCode))
          .findFirst();
      
      // "SHORT"가 없고 "PRACTICAL"이 있으면 "PRACTICAL" 사용
      String actualStepCode = stepCode; // effectively final 변수
      if (foundOpt.isEmpty() && "SHORT".equals(stepCode)) {
        foundOpt = steps.stream()
            .filter(x -> x.getStepCode().equals("PRACTICAL"))
            .findFirst();
        if (foundOpt.isPresent()) {
          // "PRACTICAL"을 찾았으면 "PRACTICAL"로 반환하도록 actualStepCode 수정
          actualStepCode = "PRACTICAL";
        }
      }
      
      if (foundOpt.isEmpty()) {
        continue; // 이 단계가 없으면 다음 단계로
      }
      
      var found = foundOpt.get();
      
      // READY 상태인 단계를 찾음 (IN_PROGRESS는 현재 진행 중인 단계이므로 제외)
      if ("READY".equals(found.getStatus())) {
        // REVIEW_WRONG 단계이고 오답이 없으면 건너뛰기
        if ("REVIEW_WRONG".equals(actualStepCode) && !hasWrongAnswers) {
          continue;
        }
        return actualStepCode; // 실제 존재하는 stepCode 반환 (SHORT 또는 PRACTICAL)
      }
    }
    
    return null; // 다음 단계 없음
  }

  /**
   * 현재 단계에 오답이 있는지 확인
   */
  private boolean hasWrongAnswers(LearningStep step) {
    String metadataJson = step.getMetadataJson();
    if (metadataJson == null || metadataJson.isBlank()) {
      return false;
    }
    
    try {
      Map<String, Object> metadata = objectMapper.readValue(
          metadataJson, new TypeReference<Map<String, Object>>() {});
      
      Object wrongIdsObj = metadata.get("wrongQuestionIds");
      if (wrongIdsObj instanceof List<?> wrongIds) {
        return !wrongIds.isEmpty();
      }
      
      return false;
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * JSON 문자열을 Map으로 파싱
   */
  private Map<String, Object> parseMetadata(String json) {
    if (json == null || json.isBlank()) {
      return Map.of();
    }
    try {
      return objectMapper.readValue(json, new TypeReference<Map<String, Object>>() {});
    } catch (Exception e) {
      return Map.of();
    }
  }

  /**
   * 학습 세션 조회 또는 생성
   * - IN_PROGRESS 상태인 세션만 재사용 (이어하기)
   * - DONE 상태이거나 없으면 새로 생성
   */
  @Transactional
  public LearningSession getOrCreateLearningSession(String userId, Long topicId, ExamMode examMode) {
    String modeStr = examMode.name();
    
    // IN_PROGRESS 상태인 세션만 재사용 (이어하기)
    var existing = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(userId, topicId, modeStr)
        .filter(s -> "IN_PROGRESS".equals(s.getStatus()));
    
    if (existing.isPresent()) {
      return existing.get();
    }
    
    // 없거나 DONE 상태면 새로 생성
    var session = sessionRepo.save(LearningSession.builder()
        .userId(userId)
        .topicId(topicId)
        .mode(modeStr)
        .status("IN_PROGRESS")
        .trulyCompleted(null)  // 초기값 null (미완료)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 단계 초기화 및 문제 사전 할당
    for (String stepCode : orderOf(examMode)) {
      LearningStep step = stepRepo.save(LearningStep.builder()
          .learningSession(session)
          .stepCode(stepCode)
          .status("READY")
          .createdAt(Instant.now())
          .updatedAt(Instant.now())
          .build());
      
      // MINI, MCQ 단계의 경우 문제 사전 할당
      if ("MINI".equals(stepCode)) {
        // MINI 단계는 필기/실기 모드 모두 OX 문제 사용
        sessionManager.createAndAllocateSessionForStep(
            step, userId, topicId, examMode, QuestionType.OX, 4);
      } else if ("MCQ".equals(stepCode)) {
        sessionManager.createAndAllocateSessionForStep(
            step, userId, topicId, examMode, QuestionType.MCQ, 5);
      }
    }
    
    // CONCEPT 단계를 첫 단계로 설정
    LearningStep conceptStep = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "CONCEPT")
        .orElse(null);
    if (conceptStep != null) {
      conceptStep.setStatus("IN_PROGRESS");
      stepRepo.save(conceptStep);
    }
    
    return session;
  }

  /**
   * 특정 단계 조회
   */
  @Transactional(readOnly = true)
  public LearningStep getStep(LearningSession session, String stepCode) {
    return stepRepo.findByLearningSessionIdAndStepCode(session.getId(), stepCode)
        .orElseThrow(() -> new NoSuchElementException("Step not found: " + stepCode));
  }

  /**
   * 단계 상태 업데이트
   */
  @Transactional
  public void updateStepStatus(LearningStep step, String status, Integer scorePct, String metadataJson) {
    step.setStatus(status);
    if (scorePct != null) {
      step.setScorePct(scorePct);
    }
    if (metadataJson != null) {
      step.setMetadataJson(metadataJson);
    }
    step.setUpdatedAt(Instant.now());
    stepRepo.save(step);
  }

  /**
   * LearningSession 저장
   */
  @Transactional
  public void saveLearningSession(LearningSession session) {
    session.setUpdatedAt(Instant.now());
    sessionRepo.save(session);
  }

  /**
   * Review 모드 학습 세션 조회 또는 생성
   * - rootTopicId 기반으로 하위 토픽들을 포함하여 문제 선택
   * - 단계 순서: MCQ -> REVIEW_WRONG -> SUMMARY
   */
  @Transactional
  public LearningSession getOrCreateReviewLearningSession(String userId, Long rootTopicId, ExamMode examMode) {
    String modeStr = "REVIEW";
    
    // IN_PROGRESS 상태인 세션만 재사용 (이어하기)
    // Review 모드는 rootTopicId를 topicId에 저장
    var existing = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(userId, rootTopicId, modeStr)
        .filter(s -> "IN_PROGRESS".equals(s.getStatus()));
    
    if (existing.isPresent()) {
      return existing.get();
    }
    
    // 없거나 DONE 상태면 새로 생성
    var session = sessionRepo.save(LearningSession.builder()
        .userId(userId)
        .topicId(rootTopicId)  // Review 모드는 rootTopicId를 topicId에 저장
        .mode(modeStr)
        .status("IN_PROGRESS")
        .trulyCompleted(null)  // 초기값 null (미완료)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 하위 토픽들 조회 (REVIEW 모드: 직접 자식만 조회)
    // REVIEW 모드는 2레벨 토픽에서 실행되며, 자식(3레벨) 토픽들의 문제를 사용합니다.
    String examModeStr = examMode != null ? examMode.name() : null;
    Set<Long> topicIds = topicTreeService.childrenOf(rootTopicId, examModeStr);
    
    if (topicIds.isEmpty() || topicIds.equals(Set.of(rootTopicId))) {
      // 자식 토픽을 찾지 못한 경우: cert-service API 호출 실패 또는 실제로 자식이 없음
      // 이 경우 fallback으로 rootTopicId를 사용하지 않고, 명확한 에러를 발생시킴
      throw new IllegalStateException(
          String.format("REVIEW 모드: rootTopicId=%d의 자식 토픽을 찾을 수 없습니다. " +
                       "cert-service API 확인 필요 (parentId=%d, mode=%s)", 
                       rootTopicId, rootTopicId, examModeStr));
    }

    // 단계 초기화 및 문제 사전 할당 (REVIEW 모드: MCQ -> REVIEW_WRONG -> SUMMARY)
    // 필기 Review 모드용 (기본값)
    for (String stepCode : ORDER_REVIEW_WRITTEN) {
      LearningStep step = stepRepo.save(LearningStep.builder()
          .learningSession(session)
          .stepCode(stepCode)
          .status("READY")
          .createdAt(Instant.now())
          .updatedAt(Instant.now())
          .build());
      
      // MCQ 단계의 경우 문제 사전 할당 (rootTopicId와 하위 토픽들에서 선택)
      if ("MCQ".equals(stepCode)) {
        sessionManager.createAndAllocateSessionForReviewStep(
            step, userId, rootTopicId, examMode, QuestionType.MCQ, 10, topicIds);
      }
      // REVIEW_WRONG, SUMMARY는 문제 할당 없음
    }
    
    // MCQ 단계를 첫 단계로 설정 (REVIEW 모드는 CONCEPT 없음)
    LearningStep mcqStep = stepRepo.findByLearningSessionIdAndStepCode(session.getId(), "MCQ")
        .orElse(null);
    if (mcqStep != null) {
      mcqStep.setStatus("IN_PROGRESS");
      stepRepo.save(mcqStep);
    }
    
    return session;
  }
}

