package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.service.AssistPracticalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 보조학습 – 실기(Short/Long) 전용 컨트롤러
 * - 세트 시작은 GET
 * - 제출은 POST
 */
@Tag(name = "Assist Practical(보조학습 – 실기)")
@RestController
@RequestMapping("/api/study/assist/practical")
@RequiredArgsConstructor
public class AssistPracticalController {

  private final AssistPracticalService practicalService;

  @Operation(summary = "실기: 카테고리 기반 보조학습 세트 시작 (토픽 배열 선택)")
  @GetMapping("/category")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(
      @RequestParam List<Long> topicIds,
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    // 세션 생성 및 문제 반환 (learningSessionId 포함)
    return practicalService.startByCategory(topicIds, count);
  }

  @Operation(summary = "실기: 카테고리 기반 보조학습 문제 가져오기 (세션 기반)")
  @GetMapping("/category/{learningSessionId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getCategorySet(
      @PathVariable Long learningSessionId
  ) {
    return practicalService.getCategorySet(learningSessionId);
  }

  @Operation(summary = "실기: 난이도 기반 보조학습 세트 시작 (세션 기반)")
  @GetMapping("/difficulty")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(
      @RequestParam(required = false) Difficulty difficulty,
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    // 세션 생성 및 문제 반환 (learningSessionId 포함)
    return practicalService.startByDifficulty(difficulty, count);
  }

  @Operation(summary = "실기: 난이도 기반 보조학습 문제 가져오기 (세션 기반)")
  @GetMapping("/difficulty/{learningSessionId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getDifficultySet(
      @PathVariable Long learningSessionId
  ) {
    return practicalService.getDifficultySet(learningSessionId);
  }

  @Operation(summary = "실기: 약점 보완 보조학습 세트 시작 (세션 기반)")
  @GetMapping("/weakness")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    // 세션 생성 및 문제 반환 (learningSessionId 포함)
    return practicalService.startByWeakness(count);
  }

  @Operation(summary = "실기: 약점 보완 보조학습 문제 가져오기 (세션 기반)")
  @GetMapping("/weakness/{learningSessionId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getWeaknessSet(
      @PathVariable Long learningSessionId
  ) {
    return practicalService.getWeaknessSet(learningSessionId);
  }

  @Operation(summary = "실기: 보조학습 세트 제출 (혼자풀기 채점)")
  @PostMapping("/submit")
  public FlowDtos.StepEnvelope<AssistDtos.PracticalSubmitResp> submit(
      @RequestBody @Valid AssistDtos.PracticalSubmitReq req
  ) {
    return practicalService.submit(req);
  }

  @Operation(summary = "실기: 난이도 기반 보조학습 단건 즉시 채점")
  @PostMapping("/difficulty/grade-one")
  public AssistDtos.PracticalGradeOneResp gradeOneDifficulty(
      @RequestParam Long learningSessionId,
      @RequestBody @Valid AssistDtos.PracticalGradeOneReq req
  ) {
    return practicalService.gradeOneDifficulty(learningSessionId, req);
  }

  @Operation(summary = "실기: 약점 보완 보조학습 단건 즉시 채점")
  @PostMapping("/weakness/grade-one")
  public AssistDtos.PracticalGradeOneResp gradeOneWeakness(
      @RequestParam Long learningSessionId,
      @RequestBody @Valid AssistDtos.PracticalGradeOneReq req
  ) {
    return practicalService.gradeOneDifficulty(learningSessionId, req);
  }

  @Operation(summary = "실기: 카테고리 기반 보조학습 단건 즉시 채점")
  @PostMapping("/category/grade-one")
  public AssistDtos.PracticalGradeOneResp gradeOneCategory(
      @RequestParam Long learningSessionId,
      @RequestBody @Valid AssistDtos.PracticalGradeOneReq req
  ) {
    return practicalService.gradeOneDifficulty(learningSessionId, req);
  }

  @Operation(summary = "실기: 보조학습 요약 조회")
  @GetMapping("/summary")
  public FlowDtos.StepEnvelope<AssistDtos.PracticalSummaryResp> summary(
      @RequestParam Long learningSessionId
  ) {
    return practicalService.summary(learningSessionId);
  }
}
