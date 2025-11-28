package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;
import java.util.Map;

public final class FlowDtos {

  private FlowDtos() {
  }

  @Schema(description = "학습 플로우 단계 응답 래퍼")
  public record StepEnvelope<T>(
      @Schema(description = "세션 ID (StudySession)") Long sessionId,
      @Schema(description = "학습 모드(MICRO/REVIEW/ASSIST 등)") String mode,
      @Schema(description = "현재 단계 코드") String step,
      @Schema(description = "현재 단계 상태(IN_PROGRESS/COMPLETE)") String status,
      @Schema(description = "다음 단계 코드", nullable = true) String nextStep,
      @Schema(description = "세션 공통 메타 정보", nullable = true) Map<String, Object> meta,
      @Schema(description = "실제 단계 페이로드") T payload,
      @Schema(description = "학습 세션 ID (LearningSession)", nullable = true) Long learningSessionId
  ) { }

  @Schema(description = "문항 목록 + 세션 상태를 함께 내려줄 때 사용")
  public record StepListPayload<Q, E>(
      @Schema(description = "문항 목록") List<Q> questions,
      @Schema(description = "오답 해설 또는 부가 설명", nullable = true) E extra
  ) { }
}

