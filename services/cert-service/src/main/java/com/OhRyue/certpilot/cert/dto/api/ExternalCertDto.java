package com.OhRyue.certpilot.cert.dto.api;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public final class ExternalCertDto {

  private ExternalCertDto() {}

  public record Qualification(
      String jmCd,
      String seriesCd,
      String jmNm,
      String engJmNm,
      String seriesNm,
      String implNm,
      String instiNm,
      String summary,
      String job,
      String trend,
      String career,
      String hist) {}

  public record ExamSchedule(
      Long id,
      String source,
      String implYy,
      String implSeq,
      String qualgbCd,
      String qualgbNm,
      String jmCd,
      String jmNm,
      String description,
      LocalDate docRegStartDt,
      LocalDate docRegEndDt,
      LocalDate docExamStartDt,
      LocalDate docExamEndDt,
      LocalDate docPassDt,
      LocalDate pracRegStartDt,
      LocalDate pracRegEndDt,
      LocalDate pracExamStartDt,
      LocalDate pracExamEndDt,
      LocalDate pracPassDt) {}

  public record Attachment(String name, String url) {}

  public record OpenQuestion(
      Long artlSeq,
      String title,
      String seriesCd,
      String seriesNm,
      String qualgbCd,
      String qualgbNm,
      String jmCd,
      String jmNm,
      LocalDateTime regDttm,
      LocalDateTime modDttm,
      List<Attachment> attachments) {}

  public record SyncResponse(
      String name,
      int inserted,
      int updated,
      int skipped,
      int total,
      boolean failed,
      String message) {}
}

