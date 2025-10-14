package com.OhRyue.certpilot.quiz.web;

import com.OhRyue.certpilot.quiz.web.dto.QuickGradeRequest;
import com.OhRyue.certpilot.quiz.web.dto.QuickGradeResult;
import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import com.OhRyue.certpilot.quiz.service.QuizQuickService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "QuizQuick", description = "태그 기반 퀵퀴즈 API")
@RestController
@RequestMapping("/quiz/quick")
@RequiredArgsConstructor
public class QuizQuickController {

    private final QuizQuickService service;

    @Operation(summary = "퀵퀴즈 세션 생성", description = "tags로 필터, count=3~5 권장")
    @GetMapping
    public QuickSessionDto start(@RequestParam(required = false) List<String> tags,
                                 @RequestParam(defaultValue = "5") int count) {
        return service.createSession(tags, count);
    }

    @Operation(summary = "퀵퀴즈 채점", description = "오답은 내부 wrong_note에 upsert (userId 필요)")
    @PostMapping("/grade")
    public QuickGradeResult grade(@RequestBody QuickGradeRequest req) {
        return service.grade(req);
    }
}
