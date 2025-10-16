package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnFlowService;
import com.OhRyue.certpilot.learn.web.dto.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name="LearnFlow", description="세세항목 사이클 및 세부항목 총정리")
@RestController
@RequestMapping("/learn")
@RequiredArgsConstructor
public class LearnFlowController {

  private final LearnFlowService svc;

  @Operation(summary="세세항목 시작(개념+미니체크+문제5)")
  @PostMapping("/micro/start")
  public LearnMicroStartDto startMicro(@RequestBody LearnMicroStartRequest req) {
    return svc.startMicro(req);
  }

  @Operation(summary="세부항목 총정리 시작(20문제)")
  @PostMapping("/review/start")
  public LearnReviewStartDto startReview(@RequestBody LearnReviewStartRequest req) {
    return svc.startReview(req);
  }
}
