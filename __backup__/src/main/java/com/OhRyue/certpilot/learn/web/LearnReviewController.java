package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnFlowService;
import com.OhRyue.certpilot.learn.service.LearnReviewService;
import com.OhRyue.certpilot.learn.web.dto.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "LearnReview", description = "세부항목 총정리(20문제) 시작/제출 API")
@RestController
@RequestMapping("/learn/review")
@RequiredArgsConstructor
public class LearnReviewController {

    // 시작은 기존 LearnFlowService의 startReview() 재사용
    private final LearnFlowService flowSvc;
    // 제출/채점은 LearnReviewService
    private final LearnReviewService reviewSvc;

    @Operation(summary = "Review 시작: 세부항목(레벨3) 범위 출제")
    @PostMapping("/start")
    public LearnReviewStartDto start(@RequestBody LearnReviewStartRequest req) {
        return flowSvc.startReview(req);
    }

    @Operation(summary = "Review 제출/채점 + AI 요약")
    @PostMapping("/submit")
    public LearnReviewSubmitResult submit(@RequestBody LearnReviewSubmitRequest req) {
        return reviewSvc.submitReview(req);
    }
}
