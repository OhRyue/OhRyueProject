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

  /* -------- 개념 보기 -------- */
  @Operation(summary = "실기 개념 보기")
  @GetMapping("/concept/{topicId}")
  public WrittenDtos.ConceptResp concept(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return practical.loadConcept(topicId, sessionId);
  }

  @Operation(summary = "실기 개념 보기 완료 처리")
  @PostMapping("/concept/complete")
  public void completeConcept(@RequestParam Long sessionId) {
    practical.completeConcept(sessionId);
  }

  /* -------- 미니체크(OX) -------- */
  @Operation(summary = "실기 미니체크 세트(OX 4문)")
  @GetMapping("/mini/{topicId}")
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSet> mini(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return practical.miniSet(topicId, sessionId);
  }

  @Operation(summary = "실기 미니체크 제출")
  @PostMapping("/mini/submit")
  public FlowDtos.StepEnvelope<WrittenDtos.MiniSubmitResp> submitMini(
      @RequestParam Long sessionId,
      @RequestBody @Valid WrittenDtos.MiniSubmitReq req) {
    return practical.submitMini(sessionId, req);
  }

  @Operation(summary = "실기 미니체크 단건 즉시 채점")
  @PostMapping("/mini/grade-one")
  public WrittenDtos.MiniGradeOneResp gradeOneMini(
      @RequestParam Long sessionId,
      @RequestBody @Valid WrittenDtos.MiniGradeOneReq req) {
    return practical.gradeOneMini(sessionId, req);
  }

  /* -------- 메인 문제 세트 -------- */
  @Operation(summary = "실기 문제 세트(혼합 SHORT/LONG)", description = "필수: 미니체크 통과 이후 진행")
  @GetMapping("/set/{topicId}")
  public FlowDtos.StepEnvelope<PracticalSet> set(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return practical.practicalSet(topicId, sessionId);
  }

  @Operation(summary = "실기 제출/채점(배치)", description = "60점 미만은 오답으로 간주")
  @PostMapping("/submit")
  public FlowDtos.StepEnvelope<PracticalSubmitResp> submit(
      @RequestParam Long sessionId,
      @RequestBody @Valid PracticalSubmitReq req) {
    return practical.submitPractical(sessionId, req);
  }

  /* -------- 리뷰 -------- */
  @Operation(summary = "실기 리뷰 세트(10문)", description = "루트 토픽의 모든 하위 토픽에서 선발 (SHORT 6 + LONG 4)")
  @GetMapping("/review/{rootTopicId}")
  public FlowDtos.StepEnvelope<PracticalSet> review(
      @PathVariable Long rootTopicId,
      @RequestParam Long sessionId) {
    return practical.practicalReviewSet(rootTopicId, sessionId);
  }

  @Operation(summary = "실기 리뷰 제출/채점")
  @PostMapping("/review/submit")
  public FlowDtos.StepEnvelope<PracticalReviewSubmitResp> reviewSubmit(
      @RequestBody @Valid PracticalReviewSubmitReq req) {
    return practical.practicalReviewSubmit(req);
  }

  @Operation(summary = "실기 리뷰 단건 즉시 채점")
  @PostMapping("/review/grade-one")
  public PracticalReviewGradeOneResp gradeOnePracticalReview(
      @RequestParam Long sessionId,
      @RequestBody @Valid PracticalReviewGradeOneReq req) {
    return practical.gradeOnePracticalReview(sessionId, req);
  }

  /* -------- 요약/즉시 채점 -------- */
  @Operation(summary = "실기 진행 요약", description = "평균 점수/풀이 수/AI 요약 제공")
  @GetMapping("/summary")
  public FlowDtos.StepEnvelope<WrittenDtos.SummaryResp> summary(
      @RequestParam Long topicId,
      @RequestParam Long sessionId) {
    return practical.summary(topicId, sessionId);
  }

  @Operation(summary = "실기 단건 즉시 채점")
  @PostMapping("/grade-one")
  public PracticalGradeOneResp gradeOne(
      @RequestParam Long sessionId,
      @RequestBody @Valid PracticalGradeOneReq req) {
    return practical.gradeOnePractical(sessionId, req);
  }
}
