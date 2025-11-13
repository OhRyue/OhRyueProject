package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.dto.api.ExternalCertDto;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class ExternalCertMapper {

  private static final TypeReference<List<ExternalCertDto.Attachment>> ATTACHMENT_TYPE =
      new TypeReference<>() {};

  private final ObjectMapper objectMapper;

  public ExternalCertDto.Qualification toDto(QualificationEntity entity) {
    return new ExternalCertDto.Qualification(
        entity.getJmCd(),
        entity.getSeriesCd(),
        entity.getJmNm(),
        entity.getEngJmNm(),
        entity.getSeriesNm(),
        entity.getImplNm(),
        entity.getInstiNm(),
        entity.getSummary(),
        entity.getJob(),
        entity.getTrend(),
        entity.getCareer(),
        entity.getHist()
    );
  }

  public ExternalCertDto.ExamSchedule toDto(ExamScheduleEntity entity) {
    return new ExternalCertDto.ExamSchedule(
        entity.getId(),
        entity.getSource(),
        entity.getImplYy(),
        entity.getImplSeq(),
        entity.getQualgbCd(),
        entity.getQualgbNm(),
        entity.getJmCd(),
        entity.getJmNm(),
        entity.getDescription(),
        entity.getDocRegStartDt(),
        entity.getDocRegEndDt(),
        entity.getDocExamStartDt(),
        entity.getDocExamEndDt(),
        entity.getDocPassDt(),
        entity.getPracRegStartDt(),
        entity.getPracRegEndDt(),
        entity.getPracExamStartDt(),
        entity.getPracExamEndDt(),
        entity.getPracPassDt()
    );
  }

  public ExternalCertDto.OpenQuestion toDto(OpenQuestionEntity entity) {
    return new ExternalCertDto.OpenQuestion(
        entity.getArtlSeq(),
        entity.getTitle(),
        entity.getSeriesCd(),
        entity.getSeriesNm(),
        entity.getQualgbCd(),
        entity.getQualgbNm(),
        entity.getJmCd(),
        entity.getJmNm(),
        entity.getRegDttm(),
        entity.getModDttm(),
        parseAttachments(entity.getAttachmentsJson())
    );
  }

  private List<ExternalCertDto.Attachment> parseAttachments(String json) {
    if (json == null) {
      return Collections.emptyList();
    }
    try {
      return objectMapper.readValue(json, ATTACHMENT_TYPE);
    } catch (IOException e) {
      log.debug("Failed to parse attachment json: {}", e.getMessage());
      return Collections.emptyList();
    }
  }
}

