package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.service.LearningSessionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Study Session(학습 세션)")
@RestController
@RequestMapping("/api/study/session")
@RequiredArgsConstructor
public class SessionController {

  private final LearningSessionService session;

  @Operation(summary = "세션 시작/재개")
  @PostMapping("/start")
  public StartResp start(@RequestBody @Valid StartReq req) {
    return session.start(req);
  }

  @Operation(summary = "세션 상세 조회")
  @GetMapping("/{sessionId}")
  public SessionResp get(@PathVariable Long sessionId) {
    return session.get(sessionId);
  }

  @Operation(summary = "단계 전이(현재 단계를 COMPLETE 처리)")
  @PostMapping("/advance")
  public AdvanceResp advance(@RequestBody @Valid AdvanceReq req) {
    return session.advance(req);
  }
}
