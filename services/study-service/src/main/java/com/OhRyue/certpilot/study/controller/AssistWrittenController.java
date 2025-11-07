package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.service.AssistWrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist-Written", description = "보조학습 - 필기(MCQ)")
@RestController
@RequestMapping("/api/study/assist/written")
@RequiredArgsConstructor
public class AssistWrittenController {

  private final AssistWrittenService service;

  @Operation(summary = "필기: 카테고리 보조학습(MCQ)")
  @PostMapping("/category")
  public AssistDtos.QuizSet byCategory(@Valid @RequestBody AssistDtos.CategoryStartReq req) {
    return service.startByCategory(req);
  }

  @Operation(summary = "필기: 난이도 보조학습(MCQ)")
  @PostMapping("/difficulty")
  public AssistDtos.QuizSet byDifficulty(@Valid @RequestBody AssistDtos.DifficultyStartReq req) {
    return service.startByDifficulty(req);
  }

  @Operation(summary = "필기: 약점 보완(MCQ)")
  @PostMapping("/weakness")
  public AssistDtos.QuizSet byWeakness(@Valid @RequestBody AssistDtos.WeaknessStartReq req) {
    return service.startByWeakness(req);
  }
}
