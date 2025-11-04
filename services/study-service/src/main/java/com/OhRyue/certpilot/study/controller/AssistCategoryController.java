package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.QuizDtos;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.certpilot.study.service.QuizService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Assist-Category", description = "보조학습 - 카테고리별(필기/실기)")
@RestController
@RequestMapping("/api/study/assist/category")
@RequiredArgsConstructor
public class AssistCategoryController {

    private final QuizService written;            // 필기(MCQ)
    private final PracticalService practical;     // 실기(SHORT/LONG)

    @Operation(summary = "필기: 카테고리 보조학습(MCQ)")
    @PostMapping("/written")
    public QuizDtos.QuizSet categoryWritten(@Valid @RequestBody QuizDtos.CategoryStartReq req) {
        return written.categoryQuiz(req);
    }

    @Operation(summary = "실기: 카테고리 보조학습(SHORT/LONG)")
    @PostMapping("/practical")
    public QuizDtos.QuizSet categoryPractical(@Valid @RequestBody QuizDtos.CategoryStartReq req) {
        return practical.categoryPractical(req);
    }
}
