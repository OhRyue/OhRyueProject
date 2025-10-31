package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.PracticalDtos.*;
import com.OhRyue.certpilot.study.service.StudyPracticalService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/practical")
@RequiredArgsConstructor
public class StudyPracticalController {

  private final StudyPracticalService service;

  @Operation(summary = "실기 세트(단답/서술 혼합)")
  @GetMapping("/set/{topicId}")
  public PracticalSet set(@PathVariable Long topicId,
                          @RequestParam(required = false) Integer count){
    return service.practicalSet(topicId, count);
  }

  @Operation(summary = "실기 제출/채점(LLM 점수 + 맞춤 해설)")
  @PostMapping("/submit")
  public PracticalSubmitResp submit(@RequestBody PracticalSubmitReq req){
    return service.submitPractical(req);
  }
}
