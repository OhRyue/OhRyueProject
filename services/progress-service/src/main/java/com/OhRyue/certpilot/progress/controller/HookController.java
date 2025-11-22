// 변경 없음 (참고용으로만 다시 적습니다)
package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.HookDtos.StudySubmitReq;
import com.OhRyue.certpilot.progress.dto.StudyFlowCompleteReq;
import com.OhRyue.certpilot.progress.service.StudyActivityService;
import com.OhRyue.certpilot.progress.service.StudyFlowService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Progress - Hooks", description = "학습 이벤트 수신 훅")
@RestController
@RequestMapping("/api/progress/hook")
@RequiredArgsConstructor
public class HookController {

  private final StudyActivityService studyActivityService;
  private final StudyFlowService studyFlowService;

  @Operation(summary = "스터디 서비스에서 전송되는 학습 이벤트 수신")
  @PostMapping("/submit")
  @ResponseStatus(HttpStatus.ACCEPTED)
  public void submit(@RequestBody @Valid StudySubmitReq payload) {
    studyActivityService.ingest(payload);
  }

  @PostMapping("/flow-complete")
  public void flowComplete(@RequestBody StudyFlowCompleteReq req) {
    studyFlowService.handleFlowComplete(req);
  }
}
