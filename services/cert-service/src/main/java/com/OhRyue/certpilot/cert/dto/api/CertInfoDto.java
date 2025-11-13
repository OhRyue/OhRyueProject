package com.OhRyue.certpilot.cert.dto.api;

import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public final class CertInfoDto {

  private CertInfoDto() {
  }

  @Schema(description = "사용자 목표 자격증 정보")
  public record GoalSummary(
      @Schema(description = "목표 ID") Long id,
      @Schema(description = "사용자 ID") String userId,
      @Schema(description = "자격증 ID") Long certId,
      @Schema(description = "목표 시험 모드") String targetExamMode,
      @Schema(description = "목표 회차 ID") Long targetRoundId,
      @Schema(description = "D-Day", example = "120") Integer dday
  ) {}

  @Schema(description = "자격증 기본 정보")
  public record CertSummary(
      Long id,
      String name,
      String level,
      String issuer,
      String issuerUrl,
      String description,
      Integer writtenFee,
      Integer practicalFee,
      String passRuleText,
      String qnetJmCd
  ) {}

  @Schema(description = "과목 정보")
  public record SubjectSummary(
      String examMode,
      String name,
      Integer totalQuestions,
      Integer durationMinutes
  ) {}

  @Schema(description = "큐넷 자격 상세 정보")
  public record QualificationSummary(
      String jmCd,
      String jmNm,
      String engJmNm,
      String seriesCd,
      String seriesNm,
      String implNm,
      String instiNm,
      String summary,
      String job,
      String trend,
      String career,
      String hist
  ) {}

  @Schema(description = "시험 일정 정보")
  public record ExamScheduleSummary(
      String implYy,
      String implSeq,
      String qualgbNm,
      String docRegStartDt,
      String docRegEndDt,
      String docExamStartDt,
      String docExamEndDt,
      String docPassDt,
      String pracRegStartDt,
      String pracRegEndDt,
      String pracExamStartDt,
      String pracExamEndDt,
      String pracPassDt
  ) {}

  @Schema(description = "현재 목표 자격증 응답")
  public record CurrentCertResponse(
      GoalSummary goal,
      CertSummary cert,
      QualificationSummary qualification,
      @ArraySchema(schema = @Schema(implementation = SubjectSummary.class))
      List<SubjectSummary> writtenSubjects,
      @ArraySchema(schema = @Schema(implementation = SubjectSummary.class))
      List<SubjectSummary> practicalSubjects,
      @ArraySchema(schema = @Schema(implementation = ExamScheduleSummary.class))
      List<ExamScheduleSummary> schedules
  ) {}

  @Schema(description = "큐넷 공개문제 요약")
  public record OpenQuestionSummary(
      Long artlSeq,
      String title,
      String regDttm,
      List<Attachment> attachments
  ) {}

  @Schema(description = "첨부 파일")
  public record Attachment(
      String name,
      String url
  ) {}

  @Schema(description = "자격증 학습 팁 응답")
  public record TipsResponse(
      String jmCd,
      QualificationSummary qualification,
      @ArraySchema(schema = @Schema(implementation = String.class))
      List<String> recommendedKeywords,
      @ArraySchema(schema = @Schema(implementation = OpenQuestionSummary.class))
      List<OpenQuestionSummary> recentOpenQuestions,
      String message
  ) {}
}

