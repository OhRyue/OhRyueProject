package com.OhRyue.certpilot.learn.web;

import com.OhRyue.certpilot.learn.service.LearnReviewService;
import com.OhRyue.certpilot.learn.web.dto.ReviewGradeRequest;
import com.OhRyue.certpilot.learn.web.dto.ReviewGradeResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name="LearnReview")
@RestController
@RequestMapping("/learn/review")
@RequiredArgsConstructor
public class LearnReviewController {

  private final LearnReviewService svc;

  @Operation(summary = "총정리 채점 + AI 요약")
  @PostMapping("/grade")
  public ReviewGradeResult grade(@RequestBody ReviewGradeRequest req) {
    return svc.gradeReview(req);
  }
}
