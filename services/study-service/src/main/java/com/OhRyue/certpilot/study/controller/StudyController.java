package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.MicroDtos.*;
import com.OhRyue.certpilot.study.service.StudyFlowService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/main")
@RequiredArgsConstructor
public class StudyController {

  private final StudyFlowService svc;

  /* 개념 */
  @GetMapping("/{topicId}/concept")
  public ConceptResp concept(@PathVariable Long topicId) {
    return svc.loadConcept(topicId);
  }

  /* 미니체크(OX) */
  @GetMapping("/{topicId}/mini")
  public MiniSet mini(@PathVariable Long topicId) {
    return svc.miniSet(topicId); }

  @PostMapping("/mini/submit")
  public MiniSubmitResp submitMini(@Valid @RequestBody MiniSubmitReq req) {
    return svc.submitMini(req);
  }

  /* 객관식 */
  @GetMapping("/{topicId}/mcq")
  public McqSet mcq(@PathVariable Long topicId, @RequestParam String userId) {
    return svc.mcqSet(topicId, userId);
  }

  @PostMapping("/mcq/submit")
  public McqSubmitResp submitMcq(@Valid @RequestBody McqSubmitReq req) {

    return svc.submitMcq(req);
  }

  /* 요약 */
  @GetMapping("/{topicId}/summary")
  public SummaryResp summary(@RequestParam String userId, @PathVariable Long topicId) {
    return svc.summary(userId, topicId);
  }
}
