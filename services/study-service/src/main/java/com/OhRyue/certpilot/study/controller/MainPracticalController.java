package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.PracticalDtos.*;
import com.OhRyue.certpilot.study.dto.WrittenDtos;
import com.OhRyue.certpilot.study.service.PracticalService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Main - Practical(실기)")
@RestController
@RequestMapping("/api/study/practical")
@RequiredArgsConstructor
public class MainPracticalController {

    private final PracticalService practical;

    /* -------- 미니체크(OX) -------- */
    @Operation(summary = "실기 미니체크 세트(OX 4문)")
    @GetMapping("/mini/{topicId}")
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> mini(@PathVariable Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return practical.miniSet(topicId);
    }

    @Operation(summary = "실기 미니체크 제출")
    @PostMapping("/mini/submit")
    public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(
            @RequestBody @Valid WrittenDtos.MiniSubmitReq req) {
        return practical.submitMini(req);
    }

    @Operation(summary = "실기 미니체크 단건 즉시 채점")
    @PostMapping("/mini/grade-one")
    public WrittenDtos.MiniGradeOneResp gradeOneMini(
            @RequestBody @Valid WrittenDtos.MiniGradeOneReq req) {
        return practical.gradeOneMini(req);
    }

    /* -------- 메인 문제 세트 -------- */
    @Operation(summary = "실기 문제 세트(혼합 SHORT/LONG)", description = "필수: 미니체크 통과 이후 진행")
    @GetMapping("/set/{topicId}")
    public FlowDtos.StepEnvelope<PracticalSet> set(@PathVariable Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return practical.practicalSet(topicId);
    }

    @Operation(summary = "실기 제출/채점(배치)", description = "60점 미만은 오답으로 간주")
    @PostMapping("/submit")
    public FlowDtos.StepEnvelope<PracticalSubmitResp> submit(
            @RequestBody @Valid PracticalSubmitReq req) {
        return practical.submitPractical(req);
    }

    /* -------- 리뷰 -------- */
    @Operation(summary = "실기 리뷰 세트(20문)", description = "루트 토픽의 모든 하위 토픽에서 선발")
    @GetMapping("/review/{rootTopicId}")
    public FlowDtos.StepEnvelope<PracticalSet> review(@PathVariable Long rootTopicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return practical.practicalReviewSet(rootTopicId);
    }

    @Operation(summary = "실기 리뷰 제출/채점")
    @PostMapping("/review/submit")
    public FlowDtos.StepEnvelope<PracticalReviewSubmitResp> reviewSubmit(
            @RequestBody @Valid PracticalReviewSubmitReq req) {
        return practical.practicalReviewSubmit(req);
    }

    /* -------- 요약/즉시 채점 -------- */
    @Operation(summary = "실기 진행 요약", description = "평균 점수/풀이 수/AI 요약 제공")
    @GetMapping("/summary")
    public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(@RequestParam Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return practical.summary(topicId);
    }

    @Operation(summary = "실기 단건 즉시 채점")
    @PostMapping("/grade-one")
    public PracticalGradeOneResp gradeOne(@RequestBody @Valid PracticalGradeOneReq req) {
        return practical.gradeOnePractical(req);
    }
}
