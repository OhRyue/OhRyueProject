package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.AssistDtos.*;
import com.OhRyue.certpilot.study.service.AssistPracticalService;
import com.OhRyue.certpilot.study.service.AssistWrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist(보조학습)")
@RestController
@RequestMapping("/api/study/assist")
@RequiredArgsConstructor
public class AssistController {

  private final AssistWrittenService written;
  private final AssistPracticalService practical;

  /* -------- 필기(MCQ) -------- */

  @Operation(summary = "필기: 카테고리 기반 보조학습 시작")
  @PostMapping("/written/category")
  public QuizSet writtenByCategory(@RequestBody @Valid CategoryStartReq req) {
    return written.startByCategory(req);
  }

  @Operation(summary = "필기: 난이도 기반 보조학습 시작")
  @PostMapping("/written/difficulty")
  public QuizSet writtenByDifficulty(@RequestBody @Valid DifficultyStartReq req) {
    return written.startByDifficulty(req);
  }

  @Operation(summary = "필기: 약점 보완 보조학습 시작")
  @PostMapping("/written/weakness")
  public QuizSet writtenByWeakness(@RequestBody @Valid WeaknessStartReq req) {
    return written.startByWeakness(req);
  }

  /* -------- 실기(Short/Long) -------- */

  @Operation(summary = "실기: 카테고리 기반 보조학습 시작")
  @PostMapping("/practical/category")
  public QuizSet practicalByCategory(@RequestBody @Valid CategoryStartReq req) {
    return practical.startByCategory(req);
  }

  @Operation(summary = "실기: 난이도 기반 보조학습 시작")
  @PostMapping("/practical/difficulty")
  public QuizSet practicalByDifficulty(@RequestBody @Valid DifficultyStartReq req) {
    return practical.startByDifficulty(req);
  }

  @Operation(summary = "실기: 약점 보완 보조학습 시작")
  @PostMapping("/practical/weakness")
  public QuizSet practicalByWeakness(@RequestBody @Valid WeaknessStartReq req) {
    return practical.startByWeakness(req);
  }
}
