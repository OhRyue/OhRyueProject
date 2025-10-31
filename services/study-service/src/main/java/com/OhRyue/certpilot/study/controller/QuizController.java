package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.QuizDtos.*;
import com.OhRyue.certpilot.study.service.QuizService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/study/aux")
@RequiredArgsConstructor
public class QuizController {

  private final QuizService svc;

  @PostMapping("/category")
  public QuizSet category(@Valid @RequestBody CategoryStartReq req) {
    return svc.categoryQuiz(req);
  }

  @PostMapping("/difficulty")
  public QuizSet difficulty(@Valid @RequestBody DifficultyStartReq req) {
    return svc.difficultyQuiz(req);
  }

  @PostMapping("/weakness")
  public QuizSet weakness(@Valid @RequestBody WeaknessStartReq req) {
    return svc.weaknessQuiz(req);
  }
}
