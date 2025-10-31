package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.LearnSession;
import com.OhRyue.certpilot.study.domain.LearnStep;
import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.repository.LearnSessionRepository;
import com.OhRyue.certpilot.study.repository.LearnStepRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;
import java.util.NoSuchElementException;

@Service
@RequiredArgsConstructor
@Transactional
public class StudySessionService {

  private final LearnSessionRepository sessionRepo;
  private final LearnStepRepository stepRepo;

  /** 고정 단계 순서(필기 기준) */
  private static final List<String> ORDER = List.of(
      "CONCEPT", "MINI", "REVIEW_WRONG", "MCQ", "REVIEW_WRONG2"
  );

  /** 세션 시작(또는 resume=true면 최신 세션 재개) */
  public StartResp start(StartReq req) {
    if (Boolean.TRUE.equals(req.resume())) {
      var found = sessionRepo.findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(
          req.userId(), req.topicId(), req.mode());
      if (found.isPresent()) {
        var s = found.get();
        s.setUpdatedAt(Instant.now());
        return new StartResp(s.getId(), s.getStatus());
      }
    }
    // 새 세션
    var s = sessionRepo.save(LearnSession.builder()
        .userId(req.userId())
        .topicId(req.topicId())
        .mode(req.mode())
        .status("IN_PROGRESS")
        .progressJson(null)
        .startedAt(Instant.now())
        .updatedAt(Instant.now())
        .build());

    // 전 단계 READY 생성
    for (String st : ORDER) {
      stepRepo.save(LearnStep.builder()
          .sessionId(s.getId())
          .step(st)
          .state("READY")
          .score(null)
          .detailsJson(null)
          .build());
    }
    return new StartResp(s.getId(), s.getStatus());
  }

  @Transactional(readOnly = true)
  public SessionResp get(Long sessionId) {
    var s = sessionRepo.findById(sessionId).orElseThrow(() -> new NoSuchElementException("session not found"));
    var steps = stepRepo.findBySessionIdOrderByIdAsc(sessionId).stream()
        .map(x -> new SessionResp.StepItem(x.getId(), x.getStep(), x.getState(), x.getScore(), x.getDetailsJson()))
        .toList();
    return new SessionResp(s.getId(), s.getUserId(), s.getTopicId(), s.getMode(),
        s.getStatus(), s.getProgressJson(), steps);
  }

  /** 현재 step을 PASS 처리하고 다음 READY 단계로 이동. 없으면 DONE */
  public AdvanceResp advance(AdvanceReq req) {
    var s = sessionRepo.findById(req.sessionId()).orElseThrow(() -> new NoSuchElementException("session not found"));

    var steps = stepRepo.findBySessionIdOrderByIdAsc(s.getId());
    var current = steps.stream().filter(st -> st.getStep().equals(req.step()))
        .findFirst().orElseThrow(() -> new NoSuchElementException("step not found in session"));

    current.setState("PASS");
    current.setScore(req.score());
    current.setDetailsJson(req.detailsJson());
    stepRepo.save(current);

    String next = null;
    for (String st : ORDER) {
      var found = steps.stream().filter(x -> x.getStep().equals(st)).findFirst().orElseThrow();
      if ("READY".equals(found.getState())) { next = st; break; }
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
