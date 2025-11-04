package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.QuizDtos;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.certpilot.study.service.QuizService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist-Difficulty", description = "보조학습 - 난이도별(필기/실기)")
@RestController
@RequestMapping("/api/study/assist/difficulty")
@RequiredArgsConstructor
public class AssistDifficultyController {

    private final QuizService written;
    private final PracticalService practical;

    @Operation(summary = "필기: 난이도 보조학습(MCQ)")
    @PostMapping("/written")
    public QuizDtos.QuizSet difficultyWritten(@Valid @RequestBody QuizDtos.DifficultyStartReq req) {
        return written.difficultyQuiz(req);
    }

    @Operation(summary = "실기: 난이도 보조학습(SHORT/LONG)")
    @PostMapping("/practical")
    public QuizDtos.QuizSet difficultyPractical(@Valid @RequestBody QuizDtos.DifficultyStartReq req) {
        return practical.difficultyPractical(req);
    }
}
