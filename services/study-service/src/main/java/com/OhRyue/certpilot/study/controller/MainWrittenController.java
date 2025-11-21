package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.WrittenDtos.*;
import com.OhRyue.certpilot.study.service.WrittenService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Main - Written(필기)")
@RestController
@RequestMapping("/api/study/written")
@RequiredArgsConstructor
public class MainWrittenController {

    private final WrittenService written;

    /* ========= 필기학습: 미니체크(OX) ========= */
    @Operation(summary = "미니체크 세트(4문) 불러오기")
    @GetMapping("/mini/{topicId}")
    public FlowDtos.StepEnvelope<MiniSet> miniSet(@PathVariable Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return written.miniSet(topicId);
    }

    @Operation(summary = "미니체크 제출", description = "전부 정답 시 miniPassed=true로 진행도 반영")
    @PostMapping("/mini/submit")
    public FlowDtos.StepEnvelope<MiniSubmitResp> submitMini(@RequestBody @Valid MiniSubmitReq req) {
        return written.submitMini(req);
    }

    @Operation(summary = "OX 단건 즉시 채점")
    @PostMapping("/mini/grade-one")
    public MiniGradeOneResp gradeOneMini(@RequestBody @Valid MiniGradeOneReq req) {
        return written.gradeOneMini(req);
    }

    /* ========= 필기학습: 객관식(MCQ) ========= */
    @Operation(summary = "MCQ 세트(5문) 불러오기")
    @GetMapping("/mcq/{topicId}")
    public FlowDtos.StepEnvelope<McqSet> mcqSet(@PathVariable Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return written.mcqSet(topicId);
    }

    @Operation(summary = "MCQ 제출", description = "정답/오답 기록 저장 및 AI 오답 해설 제공")
    @PostMapping("/mcq/submit")
    public FlowDtos.StepEnvelope<McqSubmitResp> submitMcq(@RequestBody @Valid McqSubmitReq req) {
        return written.submitMcq(req);
    }

    @Operation(summary = "MCQ 단건 즉시 채점")
    @PostMapping("/mcq/grade-one")
    public McqGradeOneResp gradeOneMcq(@RequestBody @Valid McqGradeOneReq req) {
        return written.gradeOneMcq(req);
    }

    /* ========= 필기학습: 요약 ========= */
    @Operation(summary = "필기 학습 요약", description = "미니/MCQ 결과를 바탕으로 완료 여부 및 AI 요약을 제공합니다.")
    @GetMapping("/summary")
    public FlowDtos.StepEnvelope<SummaryResp> summary(@RequestParam Long topicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return written.summary(topicId);
    }

    /* ========= 필기학습: 리뷰(총정리) ========= */
    @Operation(summary = "필기 리뷰 세트(20문) - 루트 토픽 기준 하위 토픽 전체")
    @GetMapping("/review/{rootTopicId}")
    public FlowDtos.StepEnvelope<com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet> review(
            @PathVariable Long rootTopicId) {
        String userId = AuthUserUtil.getCurrentUserId();
        return written.reviewSet(rootTopicId);
    }

    @Operation(summary = "필기 리뷰 제출(= 객관식 제출과 동일)")
    @PostMapping("/review/submit")
    public FlowDtos.StepEnvelope<McqSubmitResp> reviewSubmit(@RequestParam Long rootTopicId,
                                                             @RequestBody @Valid McqSubmitReq req) {
        return written.reviewSubmitWritten(req, rootTopicId);
    }
}
