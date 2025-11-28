package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

public class SessionDtos {

  @Schema(description = "세션 시작/재개 요청")
  public record StartReq(
      Long topicId,
      String mode,   // WRITTEN | PRACTICAL | REVIEW
      Boolean resume, // true면 최근 세션 재개
      String examMode // REVIEW 모드일 때 필수: WRITTEN | PRACTICAL (실기 Review는 PRACTICAL)
  ) {}

  @Schema(description = "세션 시작/재개 응답")
  public record StartResp(Long sessionId, String status) {}

  @Schema(description = "세션 상세 조회 응답")
  public record SessionResp(
      Long sessionId,
      Long topicId,
      String mode,
      String status,
      String currentStep,  // 현재 진행 중인 단계
      List<StepItem> steps
  ) {
    public record StepItem(Long id, String step, String state, Integer score, String detailsJson) {}
  }

  @Schema(description = "단계 전이 요청(현재 단계를 PASS 처리)")
  public record AdvanceReq(
      Long sessionId,
      String step,          // CONCEPT | MINI | REVIEW_WRONG | MCQ | REVIEW_WRONG2
      Integer score,        // 정답률 등
      String detailsJson    // 추가 메타(JSON 문자)
  ) {}

  @Schema(description = "단계 전이 응답")
  public record AdvanceResp(Long sessionId, String status, String movedTo) {}
}
