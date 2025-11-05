package com.OhRyue.certpilot.study.dto;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

import java.util.List;

/**
 * 보조학습(Assist) 공용 DTO
 * - Written(MCQ) / Practical(Short/Long) 공통 스타트 요청과 응답 구조
 * - 신규 개발은 이 DTO만 사용 권장
 */
public class AssistDtos {

  /* ===================== Start Requests ===================== */

  @Schema(description = "카테고리(주제) 기반 시작 요청: topicIds 범위에서 n문제")
  public record CategoryStartReq(
      @Schema(description = "출제 범위 토픽 ID 목록") List<Long> topicIds,
      @Schema(description = "출제 문항 수(권장: 10/20/50)") int count
  ) {}

  @Schema(description = "난이도 기반 시작 요청: 선택 난이도에서 n문제")
  public record DifficultyStartReq(
      @Schema(description = "난이도(EASY|NORMAL|HARD)") @NotNull Difficulty difficulty,
      @Schema(description = "출제 문항 수(권장: 10/20/50)") int count
  ) {}

  @Schema(description = "약점 보완 시작 요청: 최근 통계로 약한 토픽/난이도를 추출해 n문제")
  public record WeaknessStartReq(
      @Schema(description = "대상 사용자 ID") @NotNull String userId,
      @Schema(description = "출제 문항 수(권장: 10/20/50)") int count
  ) {}

  /* ===================== Quiz Payloads ===================== */

  @Schema(description = "보조학습 세트 응답")
  public record QuizSet(
      @Schema(description = "문항 목록") List<QuizQ> items
  ) {}

  @Schema(description = "보조학습 문제 1개")
  public record QuizQ(
      @Schema(description = "문항 ID") Long questionId,
      @Schema(description = "문항 본문") String text,
      @Schema(description = "선택지 목록(실기는 빈 리스트)") List<Choice> choices,
      @Schema(description = "문항 이미지 URL(선택)") String imageUrl
  ) {}

  @Schema(description = "객관식 선택지 (Written 전용)")
  public record Choice(
      @Schema(description = "라벨(A/B/C/D ...)") String label,
      @Schema(description = "선택지 텍스트") String text
  ) {}
}
