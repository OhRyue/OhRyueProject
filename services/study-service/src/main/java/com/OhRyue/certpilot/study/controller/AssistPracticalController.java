package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.service.AssistPracticalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist-Practical", description = "보조학습 - 실기(SHORT/LONG)")
@RestController
@RequestMapping("/api/study/assist/practical")
@RequiredArgsConstructor
public class AssistPracticalController {

  private final AssistPracticalService service;

  @Operation(summary = "실기: 카테고리 보조학습(Short/Long)")
  @PostMapping("/category")
  public AssistDtos.QuizSet byCategory(@Valid @RequestBody AssistDtos.CategoryStartReq req) {
    return service.startByCategory(req);
  }

  @Operation(summary = "실기: 난이도 보조학습(Short/Long)")
  @PostMapping("/difficulty")
  public AssistDtos.QuizSet byDifficulty(@Valid @RequestBody AssistDtos.DifficultyStartReq req) {
    return service.startByDifficulty(req);
  }

  @Operation(summary = "실기: 약점 보완(Short/Long)")
  @PostMapping("/weakness")
  public AssistDtos.QuizSet byWeakness(@Valid @RequestBody AssistDtos.WeaknessStartReq req) {
    return service.startByWeakness(req);
  }
}
