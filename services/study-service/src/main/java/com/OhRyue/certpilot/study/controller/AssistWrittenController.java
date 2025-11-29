package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.dto.AssistDtos;
import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.service.AssistWrittenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * 보조학습 – 필기(MCQ) 전용 컨트롤러
 * - 세트 시작은 GET
 * - 제출은 POST
 */
@Tag(name = "Assist Written(보조학습 – 필기)")
@RestController
@RequestMapping("/api/study/assist/written")
@RequiredArgsConstructor
public class AssistWrittenController {

  private final AssistWrittenService writtenService;

  @Operation(summary = "필기: 카테고리 기반 보조학습 세트 시작 (2레벨 토픽 선택)")
  @GetMapping("/category/{rootTopicId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByCategory(
      @PathVariable Long rootTopicId,
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    return writtenService.startByCategory(rootTopicId, count);
  }

  @Operation(summary = "필기: 난이도 기반 보조학습 세트 시작 (세션 기반)")
  @GetMapping("/difficulty")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByDifficulty(
      @RequestParam(required = false) Difficulty difficulty,
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    // 세션 생성 및 문제 반환 (learningSessionId 포함)
    return writtenService.startByDifficulty(difficulty, count);
  }

  @Operation(summary = "필기: 난이도 기반 보조학습 문제 가져오기 (세션 기반)")
  @GetMapping("/difficulty/{learningSessionId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getDifficultySet(
      @PathVariable Long learningSessionId
  ) {
    return writtenService.getDifficultySet(learningSessionId);
  }

  @Operation(summary = "필기: 약점 보완 보조학습 세트 시작 (세션 기반)")
  @GetMapping("/weakness")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> startByWeakness(
      @RequestParam(required = false) Integer count
  ) {
    // userId는 서비스 내부에서 AuthUserUtil로 조회
    // 세션 생성 및 문제 반환 (learningSessionId 포함)
    return writtenService.startByWeakness(count);
  }

  @Operation(summary = "필기: 약점 보완 보조학습 문제 가져오기 (세션 기반)")
  @GetMapping("/weakness/{learningSessionId}")
  public FlowDtos.StepEnvelope<AssistDtos.QuizSet> getWeaknessSet(
      @PathVariable Long learningSessionId
  ) {
    return writtenService.getWeaknessSet(learningSessionId);
  }

  @Operation(summary = "필기: 보조학습 세트 제출 (혼자풀기 채점)")
  @PostMapping("/submit")
  public FlowDtos.StepEnvelope<AssistDtos.WrittenSubmitResp> submit(
      @RequestBody @Valid AssistDtos.WrittenSubmitReq req
  ) {
    return writtenService.submit(req);
  }

  @Operation(summary = "필기: 난이도 기반 보조학습 단건 즉시 채점")
  @PostMapping("/difficulty/grade-one")
  public AssistDtos.WrittenGradeOneResp gradeOneDifficulty(
      @RequestParam Long learningSessionId,
      @RequestParam Long questionId,
      @RequestBody Map<String, String> body
  ) {
    String label = body.getOrDefault("label", "");
    return writtenService.gradeOneDifficulty(learningSessionId, questionId, label);
  }

  @Operation(summary = "필기: 약점 보완 보조학습 단건 즉시 채점")
  @PostMapping("/weakness/grade-one")
  public AssistDtos.WrittenGradeOneResp gradeOneWeakness(
      @RequestParam Long learningSessionId,
      @RequestParam Long questionId,
      @RequestBody Map<String, String> body
  ) {
    String label = body.getOrDefault("label", "");
    return writtenService.gradeOneDifficulty(learningSessionId, questionId, label);
  }
}
