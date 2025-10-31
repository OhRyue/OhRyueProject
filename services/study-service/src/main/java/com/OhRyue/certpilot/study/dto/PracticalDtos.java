package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

public class PracticalDtos {

  @Schema(description = "실기용 문제 1개(단답/서술)")
  public record PracticalQuestion(
      Long questionId,
      String type,          // SHORT | LONG
      String text,
      String imageUrl
  ) {}

  @Schema(description = "실기 문제 세트")
  public record PracticalSet(
      List<PracticalQuestion> items
  ) {}

  /* ---------- 제출 ---------- */

  @Schema(description = "실기 제출: 사용자 답안 1개")
  public record PracticalAnswer(
      Long questionId,
      String userText
  ) {}

  @Schema(description = "실기 제출 요청")
  public record PracticalSubmitReq(
      String userId,
      Long topicId,
      List<PracticalAnswer> answers
  ) {}

  @Schema(description = "실기 제출 결과 아이템")
  public record PracticalSubmitItem(
      Long questionId,
      Integer score,          // 0~100
      String baseExplanation, // DB 내 기본 해설
      String aiExplanation    // LLM 결과(맞춤 해설)
  ) {}

  @Schema(description = "실기 제출 응답")
  public record PracticalSubmitResp(
      int total,
      int avgScore,
      List<PracticalSubmitItem> items
  ) {}
}
