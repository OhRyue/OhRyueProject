package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.WrittenDtos;
import com.OhRyue.certpilot.study.dto.WrittenDtos.ConceptResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.MiniSet;
import com.OhRyue.certpilot.study.dto.WrittenDtos.MiniSubmitReq;
import com.OhRyue.certpilot.study.dto.WrittenDtos.MiniSubmitResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.MiniGradeOneReq;
import com.OhRyue.certpilot.study.dto.WrittenDtos.MiniGradeOneResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.McqSet;
import com.OhRyue.certpilot.study.dto.WrittenDtos.McqSubmitReq;
import com.OhRyue.certpilot.study.dto.WrittenDtos.McqSubmitResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.McqGradeOneReq;
import com.OhRyue.certpilot.study.dto.WrittenDtos.McqGradeOneResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.SummaryResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.QuestionDetailResp;
import com.OhRyue.certpilot.study.dto.WrittenDtos.QuestionDetailListResp;
import com.OhRyue.certpilot.study.service.WrittenService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Main - Written(필기)")
@RestController
@RequestMapping("/api/study/written")
@RequiredArgsConstructor
public class MainWrittenController {

  private final WrittenService written;

  /* ========= 필기학습: 개념 ========= */
  @Operation(summary = "개념 보기")
  @GetMapping("/concept/{topicId}")
  public ConceptResp loadConcept(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return written.loadConcept(topicId, sessionId);
  }

  @Operation(summary = "개념 보기 완료 처리")
  @PostMapping("/concept/complete")
  public void completeConcept(@RequestParam Long sessionId) {
    written.completeConcept(sessionId);
  }

  /* ========= 필기학습: 미니체크(OX) ========= */
  @Operation(summary = "미니체크 세트(4문) 불러오기")
  @GetMapping("/mini/{topicId}")
  public FlowDtos.StepEnvelope<MiniSet> miniSet(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return written.miniSet(topicId, sessionId);
  }

  @Operation(summary = "미니체크 제출", description = "전부 정답 시 miniPassed=true로 진행도 반영")
  @PostMapping("/mini/submit")
  public FlowDtos.StepEnvelope<MiniSubmitResp> submitMini(
      @RequestParam Long sessionId,
      @RequestBody @Valid MiniSubmitReq req) {
    return written.submitMini(sessionId, req);
  }

  @Operation(summary = "OX 단건 즉시 채점")
  @PostMapping("/mini/grade-one")
  public MiniGradeOneResp gradeOneMini(
      @RequestParam Long sessionId,
      @RequestBody @Valid MiniGradeOneReq req) {
    return written.gradeOneMini(sessionId, req);
  }

  /* ========= 필기학습: 객관식(MCQ) ========= */
  @Operation(summary = "MCQ 세트(5문) 불러오기")
  @GetMapping("/mcq/{topicId}")
  public FlowDtos.StepEnvelope<McqSet> mcqSet(
      @PathVariable Long topicId,
      @RequestParam Long sessionId) {
    return written.mcqSet(topicId, sessionId);
  }

  @Operation(summary = "MCQ 제출", description = "정답/오답 기록 저장 및 AI 오답 해설 제공")
  @PostMapping("/mcq/submit")
  public FlowDtos.StepEnvelope<McqSubmitResp> submitMcq(
      @RequestParam Long sessionId,
      @RequestBody @Valid McqSubmitReq req) {
    return written.submitMcq(sessionId, req);
  }

  @Operation(summary = "MCQ 단건 즉시 채점")
  @PostMapping("/mcq/grade-one")
  public McqGradeOneResp gradeOneMcq(
      @RequestParam Long sessionId,
      @RequestBody @Valid McqGradeOneReq req) {
    return written.gradeOneMcq(sessionId, req);
  }

  /* ========= 필기학습: 요약 ========= */
  @Operation(summary = "필기 학습 요약", description = "미니/MCQ 결과를 바탕으로 완료 여부 및 AI 요약을 제공합니다.")
  @GetMapping("/summary")
  public FlowDtos.StepEnvelope<SummaryResp> summary(
      @RequestParam Long topicId,
      @RequestParam Long sessionId) {
    return written.summary(topicId, sessionId);
  }

  /* ========= 필기학습: 리뷰(총정리) ========= */
  @Operation(summary = "필기 리뷰 세트(10문) - 루트 토픽 기준 하위 토픽 전체")
  @GetMapping("/review/{rootTopicId}")
  public FlowDtos.StepEnvelope<com.OhRyue.certpilot.study.dto.ReviewDtos.ReviewSet> review(
      @PathVariable Long rootTopicId,
      @RequestParam Long sessionId) {
    return written.reviewSet(rootTopicId, sessionId);
  }

  @Operation(summary = "필기 리뷰 제출(= 객관식 제출과 동일)")
  @PostMapping("/review/submit")
  public FlowDtos.StepEnvelope<McqSubmitResp> reviewSubmit(
      @RequestParam Long sessionId,
      @RequestBody @Valid McqSubmitReq req) {
    return written.reviewSubmitWritten(sessionId, req);
  }

  @Operation(summary = "필기 리뷰 요약", description = "Review 모드의 학습 결과를 요약하여 반환합니다. MCQ 결과를 바탕으로 완료 여부 및 AI 요약을 제공합니다.")
  @GetMapping("/review/summary")
  public FlowDtos.StepEnvelope<SummaryResp> reviewSummary(
      @RequestParam Long rootTopicId,
      @RequestParam Long sessionId) {
    return written.reviewSummary(rootTopicId, sessionId);
  }

  /* ========= 필기학습: 문제 상세 조회 ========= */
  @Operation(summary = "필기 문제 상세 조회", description = "문제 내용, 선택지(MCQ만), 정답을 반환")
  @GetMapping("/question/{questionId}")
  public QuestionDetailResp getQuestionDetail(@PathVariable Long questionId) {
    return written.getQuestionDetail(questionId);
  }

  @Operation(summary = "필기 문제 상세 조회 (여러 개)", description = "여러 문제 ID의 문제 내용, 선택지(MCQ만), 정답을 한 번에 반환")
  @GetMapping("/questions")
  public QuestionDetailListResp getQuestionDetails(@RequestParam List<Long> questionIds) {
    return written.getQuestionDetails(questionIds);
  }
}
