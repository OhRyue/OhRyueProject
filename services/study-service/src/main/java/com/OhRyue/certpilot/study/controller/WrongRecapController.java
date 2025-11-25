package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.WrongRecapDtos.WrongRecapSet;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.certpilot.study.service.WrittenService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Wrong Recap(틀린 문제 다시보기)")
@RestController
@RequestMapping("/api/study/wrong")
@RequiredArgsConstructor
public class WrongRecapController {

  private final WrittenService written;
  private final PracticalService practical;

  @Operation(summary = "내 최근 틀린 문제 다시보기(토픽 범위)", description = "limit 기본 20")
  @GetMapping("/recap")
  public WrongRecapSet recap(@RequestParam Long topicId,
                             @RequestParam(defaultValue = "20") int limit) {
    String userId = AuthUserUtil.getCurrentUserId();
    return written.wrongRecap(topicId, limit);
  }

  @Operation(summary = "특정 문제 ID 목록으로 다시보기")
  @GetMapping("/recap-by-ids")
  public WrongRecapSet recapByIds(@RequestParam String ids) {
    String userId = AuthUserUtil.getCurrentUserId();
    return written.wrongRecapByIds(ids);
  }

  @Operation(summary = "세션 기반 틀린 문제 다시보기(필기)")
  @GetMapping("/written/session")
  public WrongRecapSet recapWrittenSession(@RequestParam Long sessionId,
                                           @RequestParam(defaultValue = "MICRO_MCQ") String step) {
    String userId = AuthUserUtil.getCurrentUserId();
    return written.wrongRecapBySession(sessionId, step);
  }

  @Operation(summary = "LearningSession 기반 틀린 문제 다시보기(필기)", 
             description = "LearningSession의 MCQ 단계에서 틀린 문제만 조회")
  @GetMapping("/written/learning-session")
  public WrongRecapSet recapWrittenLearningSession(@RequestParam Long learningSessionId) {
    String userId = AuthUserUtil.getCurrentUserId();
    return written.wrongRecapByLearningSession(learningSessionId);
  }

  @Operation(summary = "세션 기반 틀린 문제 다시보기(실기)")
  @GetMapping("/practical/session")
  public WrongRecapSet recapPracticalSession(@RequestParam Long sessionId,
                                             @RequestParam(defaultValue = "PRACTICAL_SET") String step) {
    String userId = AuthUserUtil.getCurrentUserId();
    return practical.wrongRecapBySession(sessionId, step);
  }
}
