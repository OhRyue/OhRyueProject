package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.service.AssistPracticalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

/**
 * 보조학습 – 실기(Short/Long) 전용 컨트롤러
 * - 세트 시작은 GET
 * - 제출은 POST
 */
@Tag(name = "Assist Practical(보조학습 – 실기)")
@RestController
@RequestMapping("/api/study/assist/practical")
@RequiredArgsConstructor
public class AssistPracticalController {

    private final AssistPracticalService practicalService;

    @Operation(summary = "실기: 카테고리 기반 보조학습 세트 시작")
    @GetMapping("/category/{rootTopicId}")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(
            @PathVariable Long rootTopicId,
            @RequestParam(required = false) Integer count
    ) {
        // userId는 서비스 내부에서 AuthUserUtil로 조회
        return practicalService.startByCategory(rootTopicId, count);
    }

    @Operation(summary = "실기: 난이도 기반 보조학습 세트 시작")
    @GetMapping("/difficulty")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(
            @RequestParam(required = false) Difficulty difficulty,
            @RequestParam(required = false) Integer count
    ) {
        // userId는 서비스 내부에서 AuthUserUtil로 조회
        return practicalService.startByDifficulty(difficulty, count);
    }

    @Operation(summary = "실기: 약점 보완 보조학습 세트 시작")
    @GetMapping("/weakness")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(
            @RequestParam(required = false) Integer count
    ) {
        // userId는 서비스 내부에서 AuthUserUtil로 조회
        return practicalService.startByWeakness(count);
    }

    @Operation(summary = "실기: 보조학습 세트 제출 (혼자풀기 채점)")
    @PostMapping("/submit")
    public FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submit(
            @RequestBody @Valid AssistDtos.PracticalSubmitReq req
    ) {
        return practicalService.submit(req);
    }
}
