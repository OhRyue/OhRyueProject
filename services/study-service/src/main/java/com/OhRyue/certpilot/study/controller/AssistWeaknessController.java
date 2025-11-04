package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.QuizDtos;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.certpilot.study.service.QuizService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist-Weakness", description = "보조학습 - 약점 보완(필기/실기)")
@RestController
@RequestMapping("/api/study/assist/weak")
@RequiredArgsConstructor
public class AssistWeaknessController {

    private final QuizService written;
    private final PracticalService practical;

    @Operation(summary = "필기: 약점 보완(MCQ)")
    @PostMapping("/written")
    public QuizDtos.QuizSet weaknessWritten(@Valid @RequestBody QuizDtos.WeaknessStartReq req) {
        return written.weaknessQuiz(req);
    }

    @Operation(summary = "실기: 약점 보완(SHORT/LONG)")
    @PostMapping("/practical")
    public QuizDtos.QuizSet weaknessPractical(@Valid @RequestBody QuizDtos.WeaknessStartReq req) {
        return practical.weaknessPractical(req);
    }
}
