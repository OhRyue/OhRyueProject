package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.dto.external.ExamScheduleItem;
import com.OhRyue.certpilot.cert.dto.external.OpenQuestionItem;
import com.OhRyue.certpilot.cert.dto.external.QualificationItem;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

@Component
@RequiredArgsConstructor
@Slf4j
public class QnetMapper {

  private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyyMMdd", Locale.KOREA);
  private static final DateTimeFormatter DATETIME_FORMAT = DateTimeFormatter.ofPattern("yyyyMMddHHmmss", Locale.KOREA);

  private final ObjectMapper objectMapper;

  public QualificationEntity toQualification(QualificationItem item, QualificationEntity target) {
    if (target == null) {
      target = new QualificationEntity();
      target.setCreatedAt(Instant.now());
    }
    target.setJmCd(item.getJmCd());
    target.setSeriesCd(item.getSeriesCd());
    target.setJmNm(item.getJmNm());
    target.setEngJmNm(item.getEngJmNm());
    target.setSeriesNm(item.getSeriesNm());
    target.setImplNm(item.getImplNm());
    target.setInstiNm(item.getInstiNm());
    target.setSummary(item.getSummary());
    target.setJob(item.getJob());
    target.setTrend(item.getTrend());
    target.setCareer(item.getCareer());
    target.setHist(item.getHist());
    target.setUpdatedAt(Instant.now());
    return target;
  }

  public ExamScheduleEntity toExamSchedule(String source, ExamScheduleItem item, ExamScheduleEntity target) {
    if (target == null) {
      target = new ExamScheduleEntity();
      target.setCreatedAt(Instant.now());
    }
    target.setSource(source);
    target.setImplYy(item.getImplYy());
    target.setImplSeq(item.getImplSeq());
    target.setQualgbCd(item.getQualgbCd());
    target.setQualgbNm(item.getQualgbNm());
    target.setJmCd(item.getJmCd());
    target.setJmNm(item.getJmNm());
    target.setDescription(item.getDescription());
    target.setDocRegStartDt(parseDate(item.getDocRegStartDt()));
    target.setDocRegEndDt(parseDate(item.getDocRegEndDt()));
    target.setDocExamStartDt(parseDate(item.getDocExamStartDt()));
    target.setDocExamEndDt(parseDate(item.getDocExamEndDt()));
    target.setDocPassDt(parseDate(item.getDocPassDt()));
    target.setPracRegStartDt(parseDate(item.getPracRegStartDt()));
    target.setPracRegEndDt(parseDate(item.getPracRegEndDt()));
    target.setPracExamStartDt(parseDate(item.getPracExamStartDt()));
    target.setPracExamEndDt(parseDate(item.getPracExamEndDt()));
    target.setPracPassDt(parseDate(item.getPracPassDt()));
    target.setUpdatedAt(Instant.now());
    return target;
  }

  public OpenQuestionEntity toOpenQuestion(OpenQuestionItem item, OpenQuestionEntity target) {
    if (target == null) {
      target = new OpenQuestionEntity();
      target.setCreatedAt(Instant.now());
    }
    target.setArtlSeq(item.getArtlSeq());
    target.setTitle(item.getTitle());
    target.setSeriesCd(item.getSeriesCd());
    target.setSeriesNm(item.getSeriesNm());
    target.setQualgbCd(item.getQualgbCd());
    target.setQualgbNm(item.getQualgbNm());
    target.setJmCd(item.getJmCd());
    target.setJmNm(item.getJmNm());
    target.setRegDttm(parseDateTime(item.getRegDttm()));
    target.setModDttm(parseDateTime(item.getModDttm()));
    target.setAttachmentsJson(buildAttachmentJson(item.getAttachmentNames(), item.getAttachmentUrls()));
    target.setUpdatedAt(Instant.now());
    return target;
  }

  private LocalDate parseDate(String value) {
    if (!StringUtils.hasText(value)) {
      return null;
    }
    try {
      return LocalDate.parse(value.trim(), DATE_FORMAT);
    } catch (DateTimeParseException ex) {
      log.debug("Failed to parse date '{}': {}", value, ex.getMessage());
      return null;
    }
  }

  private LocalDateTime parseDateTime(String value) {
    if (!StringUtils.hasText(value)) {
      return null;
    }
    String trimmed = value.trim();
    try {
      if (trimmed.length() == 8) {
        return LocalDate.parse(trimmed, DATE_FORMAT).atStartOfDay();
      }
      return LocalDateTime.parse(trimmed, DATETIME_FORMAT);
    } catch (DateTimeParseException ex) {
      log.debug("Failed to parse datetime '{}': {}", value, ex.getMessage());
      return null;
    }
  }

  private String buildAttachmentJson(String names, String urls) {
    if (!StringUtils.hasText(urls)) {
      return null;
    }
    String[] urlArr = urls.split("\\|");
    String[] nameArr = StringUtils.hasText(names) ? names.split("\\|") : new String[urlArr.length];
    List<Attachment> attachments = new ArrayList<>();
    for (int i = 0; i < urlArr.length; i++) {
      String url = urlArr[i].trim();
      if (!StringUtils.hasText(url)) {
        continue;
      }
      String name = (i < nameArr.length) ? nameArr[i] : null;
      attachments.add(new Attachment(name, url));
    }
    if (attachments.isEmpty()) {
      return null;
    }
    try {
      return objectMapper.writeValueAsString(attachments);
    } catch (JsonProcessingException e) {
      log.warn("Failed to serialize attachment json", e);
      return null;
    }
  }

  private record Attachment(String name, String url) {}
}

