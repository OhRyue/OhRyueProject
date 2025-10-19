package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnFlowService;
import com.OhRyue.certpilot.learn.web.dto.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "LearnFlow")
@RestController
@RequestMapping("/learn")
@RequiredArgsConstructor
public class LearnFlowController {

  private final LearnFlowService svc;

  @Operation(summary = "Micro 시작: 세세항목(레벨4) 기준 개념+미니체크(OX 4)+퀴즈5")
  @PostMapping("/micro/start")
  public LearnMicroStartDto startMicro(@RequestBody LearnMicroStartRequest req) {
    return svc.startMicro(req);
  }

  @Operation(summary = "Micro 제출/채점: 미니체크=DB해설, 퀴즈=AI해설(폴백 DB)")
  @PostMapping("/micro/submit")
  public LearnMicroSubmitResult submitMicro(@RequestBody LearnMicroSubmitRequest req) {
    return svc.submitMicro(req);
  }

  @Operation(summary = "Review 시작: 세부항목(레벨3) 범위 출제")
  @PostMapping("/review/start")
  public LearnReviewStartDto startReview(@RequestBody LearnReviewStartRequest req) {
    return svc.startReview(req);
  }

  @Operation(summary = "Review 제출/채점+요약(AI)")
  @PostMapping("/review/submit")
  public LearnReviewSubmitResult submitReview(@RequestBody LearnReviewSubmitRequest req) {
    return svc.submitReview(req);
  }
}
