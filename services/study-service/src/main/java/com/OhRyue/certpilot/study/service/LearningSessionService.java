package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.domain.LearningSession;
import com.OhRyue.certpilot.study.domain.LearningStep;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.repository.LearningSessionRepository;
import com.OhRyue.certpilot.study.repository.LearningStepRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class LearningSessionService {

  private final LearningSessionRepository sessionRepo;
  private final LearningStepRepository stepRepo;

  /** 필기(WRITTEN) 단계 순서 (CONCEPT 제외 - 개념 보기는 세션과 무관) */
  private static final List<String> ORDER_WRITTEN = List.of(
      "MINI", "REVIEW_WRONG", "MCQ", "REVIEW_WRONG2", "SUMMARY"
  );
  /** 실기(PRACTICAL) 단계 순서 (CONCEPT 제외 - 개념 보기는 세션과 무관) */
  private static final List<String> ORDER_PRACTICAL = List.of(
      "MINI", "REVIEW_WRONG", "PRACTICAL", "REVIEW_WRONG2", "SUMMARY"
  );

  /** 모드에 따른 단계 순서 (ExamMode 버전) */
  private List<String> orderOf(ExamMode mode) {
    return (mode == ExamMode.PRACTICAL) ? ORDER_PRACTICAL : ORDER_WRITTEN;
  }

  /** 모드에 따른 단계 순서 (String 버전 - 오버로드) */
  private List<String> orderOf(String modeStr) {
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
   */
  @Transactional
  public StartResp start(StartReq req) {
    String userId = AuthUserUtil.getCurrentUserId();

    if (Boolean.TRUE.equals(req.resume())) {
      var found = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(
          userId, req.topicId(), String.valueOf(req.mode()));
      if (found.isPresent()) {
        var s = found.get();
        s.setUpdatedAt(Instant.now());
        return new StartResp(s.getId(), s.getStatus());
      }
    }

    // 어떤 타입(ExamMode/String) 이든 문자열로 저장
    String modeStr = String.valueOf(req.mode());

    var s = sessionRepo.save(LearningSession.builder()
        .userId(userId)
        .topicId(req.topicId())
        .mode(modeStr)
        .status("IN_PROGRESS")
        .trulyCompleted(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 요청 모드에 맞춰 단계 초기화
    for (String stepCode : orderOf(modeStr)) {
      stepRepo.save(LearningStep.builder()
          .learningSession(s)
          .stepCode(stepCode)
          .status("READY")
          .scorePct(null)
          .metadataJson(null)
          .createdAt(Instant.now())
          .updatedAt(Instant.now())
          .build());
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
    return new SessionResp(s.getId(), s.getTopicId(), s.getMode(),
        s.getStatus(), null, steps);
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

    // 문제를 다 풀어야 COMPLETE
    current.setStatus("COMPLETE");
    current.setScorePct(req.score());
    current.setMetadataJson(req.detailsJson());
    current.setUpdatedAt(Instant.now());
    stepRepo.save(current);

    // 세션의 mode(String) 기준으로 다음 단계 탐색
    String next = null;
    for (String stepCode : orderOf(s.getMode())) {
      var found = steps.stream()
          .filter(x -> x.getStepCode().equals(stepCode))
          .findFirst()
          .orElseThrow();
      if ("READY".equals(found.getStatus())) {
        next = stepCode;
        break;
      }
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

    // 단계 초기화
    for (String stepCode : orderOf(examMode)) {
      stepRepo.save(LearningStep.builder()
          .learningSession(session)
          .stepCode(stepCode)
          .status("READY")
          .createdAt(Instant.now())
          .updatedAt(Instant.now())
          .build());
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
}

