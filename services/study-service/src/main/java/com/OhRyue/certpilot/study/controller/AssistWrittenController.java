package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.service.AssistWrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

/**
 * 보조학습 – 필기(MCQ) 전용 컨트롤러
 * - 세트 시작은 GET
 * - 제출은 POST
 */
@Tag(name = "Assist Written(보조학습 – 필기)")
@RestController
@RequestMapping("/api/study/assist/written")
@RequiredArgsConstructor
public class AssistWrittenController {

    private final AssistWrittenService writtenService;

    @Operation(summary = "필기: 카테고리 기반 보조학습 세트 시작 (2레벨 토픽 선택)")
    @GetMapping("/category/{rootTopicId}")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(
            @PathVariable Long rootTopicId,
            @RequestParam(required = false) Integer count,
            @RequestParam(required = false) String userId
    ) {
        return writtenService.startByCategory(userId, rootTopicId, count);
    }

    @Operation(summary = "필기: 난이도 기반 보조학습 세트 시작")
    @GetMapping("/difficulty")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(
            @RequestParam(required = false) Difficulty difficulty,
            @RequestParam(required = false) Integer count,
            @RequestParam(required = false) String userId
    ) {
        return writtenService.startByDifficulty(userId, difficulty, count);
    }

    @Operation(summary = "필기: 약점 보완 보조학습 세트 시작")
    @GetMapping("/weakness")
    public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(
            @RequestParam String userId,
            @RequestParam(required = false) Integer count
    ) {
        return writtenService.startByWeakness(userId, count);
    }

    @Operation(summary = "필기: 보조학습 세트 제출 (혼자풀기 채점)")
    @PostMapping("/submit")
    public FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submit(
            @RequestBody @Valid AssistDtos.WrittenSubmitReq req
    ) {
        return writtenService.submit(req);
    }
}
