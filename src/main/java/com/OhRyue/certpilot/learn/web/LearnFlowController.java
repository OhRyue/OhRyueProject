package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnFlowService;
import com.OhRyue.certpilot.learn.web.dto.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "LearnFlow", description = "세세항목 Micro 루프 & 세부항목 Review API")
@RestController
@RequestMapping("/learn")
@RequiredArgsConstructor
public class LearnFlowController {

  private final LearnFlowService svc;

  @Operation(summary = "Micro 시작: 세세항목(레벨4) 기준 개념+미니체크+5문제")
  @PostMapping("/micro/start")
  public LearnMicroStartDto startMicro(@RequestBody LearnMicroStartRequest req) {
    return svc.startMicro(req);
  }

  @Operation(summary = "Review 시작: 세부항목(레벨3) 총정리 20문제")
  @PostMapping("/review/start")
  public LearnReviewStartDto startReview(@RequestBody LearnReviewStartRequest req) {
    return svc.startReview(req);
  }
}
