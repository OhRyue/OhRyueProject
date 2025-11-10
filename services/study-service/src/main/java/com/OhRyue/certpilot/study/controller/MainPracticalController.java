package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.PracticalDtos.*;
import com.OhRyue.certpilot.study.dto.WrittenDtos.SummaryResp;
import com.OhRyue.certpilot.study.service.PracticalService;
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

  @Operation(summary = "실기 문제 세트(혼합 SHORT/LONG)", description = "count 미지정 시 기본 5문")
  @GetMapping("/set/{topicId}")
  public PracticalSet set(@PathVariable Long topicId, @RequestParam(required = false) Integer count) {
    return practical.practicalSet(topicId, count);
  }

  @Operation(summary = "실기 제출/채점(배치)", description = "60점 미만은 오답으로 간주")
  @PostMapping("/submit")
  public PracticalSubmitResp submit(@RequestBody @Valid PracticalSubmitReq req) {
    return practical.submitPractical(req);
  }

  @Operation(summary = "실기 리뷰 세트(20문)", description = "루트 토픽의 모든 하위 토픽에서 선발")
  @GetMapping("/review/{rootTopicId}")
  public PracticalSet review(@PathVariable Long rootTopicId) {
    return practical.practicalReviewSet(rootTopicId);
  }

  @Operation(summary = "실기 진행 요약", description = "평균 점수/풀이 수/연속일/AI 요약 제공")
  @GetMapping("/summary")
  public SummaryResp summary(@RequestParam String userId, @RequestParam Long topicId) {
    return practical.summary(userId, topicId);
  }

  @Operation(summary = "실기 단건 즉시 채점")
  @PostMapping("/grade-one")
  public PracticalGradeOneResp gradeOne(@RequestBody @Valid PracticalGradeOneReq req) {
    return practical.gradeOnePractical(req);
  }
}
