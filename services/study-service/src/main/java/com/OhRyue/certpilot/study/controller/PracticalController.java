package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSet;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitReq;
import com.OhRyue.certpilot.study.dto.PracticalDtos.PracticalSubmitResp;
import com.OhRyue.certpilot.study.service.PracticalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Practical (실기)", description = "실기 세트/제출 API")
@RestController
@RequestMapping("/api/study/practical")
@RequiredArgsConstructor
public class PracticalController {

  private final PracticalService service;

  @Operation(summary = "실기 세트(단답/서술 혼합)", description = "topicId 범위에서 SHORT/LONG 문제를 섞어 count개 반환")
  @GetMapping("/set/{topicId}")
  public PracticalSet set(@PathVariable Long topicId,
                          @RequestParam(required = false) Integer count) {
    return service.practicalSet(topicId, count);
  }

  @Operation(summary = "실기 제출/채점", description = "LLM 기반 점수(0~100)와 맞춤 해설을 반환")
  @PostMapping("/submit")
  public PracticalSubmitResp submit(@Valid @RequestBody PracticalSubmitReq req) {
    return service.submitPractical(req);
  }
}
