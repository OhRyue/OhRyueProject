package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.SessionDtos.*;
import com.OhRyue.certpilot.study.service.StudySessionService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/flow/session")
@RequiredArgsConstructor
public class StudySessionController {

  private final StudySessionService service;

  @Operation(summary = "세션 시작 또는 재개(resume=true)")
  @PostMapping("/start")
  public StartResp start(@RequestBody StartReq req) {
    return service.start(req);
  }

  @Operation(summary = "세션 조회")
  @GetMapping("/{sessionId}")
  public SessionResp get(@PathVariable Long sessionId) {
    return service.get(sessionId);
  }

  @Operation(summary = "단계 전이(현재 단계 PASS 처리 후 다음 READY 단계로 이동)")
  @PostMapping("/advance")
  public AdvanceResp advance(@RequestBody AdvanceReq req) {
    return service.advance(req);
  }
}
