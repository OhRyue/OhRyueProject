package com.OhRyue.certpilot.study.service;

import com.OhRyue.common.auth.AuthUserUtil;
import com.OhRyue.certpilot.study.domain.LearnSession;
import com.OhRyue.certpilot.study.domain.LearnStep;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.repository.LearnSessionRepository;
import com.OhRyue.certpilot.study.repository.LearnStepRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class StudySessionService {

  private final LearnSessionRepository sessionRepo;
  private final LearnStepRepository stepRepo;

  /** 필기(WRITTEN) 단계 순서 */
  private static final List<String> ORDER_WRITTEN = List.of(
      "CONCEPT", "MINI", "REVIEW_WRONG", "MCQ", "SUMMARY"
  );
  /** 실기(PRACTICAL) 단계 순서 */
  private static final List<String> ORDER_PRACTICAL = List.of(
      "CONCEPT", "MINI", "PRACTICAL", "REVIEW_WRONG", "SUMMARY"
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
   * 세션 시작 (resume=true면 최신 세션 재개)
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

    var s = sessionRepo.save(LearnSession.builder()
        .userId(userId)
        .topicId(req.topicId())
        .mode(modeStr)                 // 엔티티에 String으로 보관
        .status("IN_PROGRESS")
        .progressJson(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 요청 모드에 맞춰 단계 초기화 (String도 안전)
    for (String st : orderOf(modeStr)) {
      stepRepo.save(LearnStep.builder()
          .sessionId(s.getId())
          .step(st)
          .state("READY")            // 초기는 모두 READY
          .score(null)
          .detailsJson(null)
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

    var steps = stepRepo.findBySessionIdOrderByIdAsc(sessionId).stream()
        .map(x -> new SessionResp.StepItem(x.getId(), x.getStep(), x.getState(), x.getScore(), x.getDetailsJson()))
        .toList();
    return new SessionResp(s.getId(), s.getTopicId(), s.getMode(),
        s.getStatus(), s.getProgressJson(), steps);
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

    var steps = stepRepo.findBySessionIdOrderByIdAsc(s.getId());
    var current = steps.stream()
        .filter(st -> st.getStep().equals(req.step()))
        .findFirst()
        .orElseThrow(() -> new NoSuchElementException("step not found in session"));

    // 문제를 다 풀어야 COMPLETE
    current.setState("COMPLETE");
    current.setScore(req.score());
    current.setDetailsJson(req.detailsJson());
    stepRepo.save(current);

    // 세션의 mode(String) 기준으로 다음 단계 탐색
    String next = null;
    for (String st : orderOf(s.getMode())) {
      var found = steps.stream()
          .filter(x -> x.getStep().equals(st))
          .findFirst()
          .orElseThrow();
      if ("READY".equals(found.getState())) {
        next = st;
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
}
