package com.OhRyue.certpilot.study.dto;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

import java.util.List;

/**
 * 보조학습(Assist) 공용 DTO
 * - Written(MCQ) / Practical(Short/Long) 공통 스타트 요청과 응답 구조
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

  /* ===================== Written Assist – 제출 DTO ===================== */

  @Schema(description = "보조학습(필기) 제출 요청")
  public record WrittenSubmitReq(
      @Schema(description = "학습 세션 ID (세션 기반 제출 시 필수)") Long learningSessionId,
      @Schema(description = "답안 목록") List<WrittenAnswer> answers
  ) {}

  @Schema(description = "보조학습(필기) 답안 1개")
  public record WrittenAnswer(
      @Schema(description = "문항 ID") Long questionId,
      @Schema(description = "사용자가 선택한 보기 라벨(A/B/C/D)") String label
  ) {}

  @Schema(description = "보조학습(필기) 제출 응답")
  public record WrittenSubmitResp(
      @Schema(description = "총 문항 수") int total,
      @Schema(description = "정답 개수") int correct,
      @Schema(description = "결과 아이템 목록") List<WrittenResultItem> items
  ) {}

  @Schema(description = "보조학습(필기) 제출 결과 1개")
  public record WrittenResultItem(
      @Schema(description = "문항 ID") Long questionId,
      @Schema(description = "정오 여부") boolean correct,
      @Schema(description = "정답 라벨") String correctLabel,
      @Schema(description = "DB 기본 해설") String explanation
  ) {}

  /* ===================== Written Assist – 단건 즉시 채점 ===================== */

  @Schema(description = "보조학습(필기) 단건 즉시 채점 응답")
  public record WrittenGradeOneResp(
      @Schema(description = "정오 여부") Boolean correct,
      @Schema(description = "정답 라벨") String correctLabel,
      @Schema(description = "DB 기본 해설") String explanation,
      @Schema(description = "AI 해설 (오답인 경우)") String aiExplanation
  ) {}

  /* ===================== Practical Assist – 제출 DTO ===================== */

  @Schema(description = "보조학습(실기) 제출 요청")
  public record PracticalSubmitReq(
      @Schema(description = "학습 세션 ID (세션 기반 제출 시 필수)") Long learningSessionId,
      @Schema(description = "답안 목록") List<PracticalAnswer> answers
  ) {}

  @Schema(description = "보조학습(실기) 답안 1개")
  public record PracticalAnswer(
      @Schema(description = "문항 ID") Long questionId,
      @Schema(description = "사용자 주관식 답변") String userText
  ) {}

  @Schema(description = "보조학습(실기) 제출 응답")
  public record PracticalSubmitResp(
      @Schema(description = "채점된 문항 수") int total,
      @Schema(description = "맞은 문제 수") int correct,
      @Schema(description = "결과 아이템 목록") List<PracticalResultItem> items
  ) {}

  @Schema(description = "보조학습(실기) 제출 결과 1개")
  public record PracticalResultItem(
      @Schema(description = "문항 ID") Long questionId,
      @Schema(description = "맞음(true) / 틀림(false)") Boolean correct,
      @Schema(description = "DB 기본 해설") String baseExplanation,
      @Schema(description = "AI 해설") String aiExplanation
  ) {}

  /* ===================== Practical Assist – 단건 즉시 채점 ===================== */

  @Schema(description = "보조학습(실기) 단건 즉시 채점 응답")
  public record PracticalGradeOneResp(
      @Schema(description = "맞음(true) / 틀림(false)") Boolean correct,
      @Schema(description = "정답") String answerKey,
      @Schema(description = "DB 기본 해설") String baseExplanation,
      @Schema(description = "AI 해설") String aiExplanation,
      @Schema(description = "AI 해설 생성 실패 여부") Boolean aiFailed
  ) {}
}
