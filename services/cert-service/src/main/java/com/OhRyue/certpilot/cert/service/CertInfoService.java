package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.client.AccountGoalClient;
import com.OhRyue.certpilot.cert.domain.CertEntity;
import com.OhRyue.certpilot.cert.domain.CertSubjectEntity;
import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import com.OhRyue.certpilot.cert.dto.api.CertInfoDto;
import com.OhRyue.certpilot.cert.repository.*;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import feign.FeignException;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CertInfoService {

  private static final TypeReference<List<CertInfoDto.Attachment>> ATTACHMENT_TYPE =
      new TypeReference<>() {};

  private final CertRepository certRepository;
  private final CertSubjectRepository certSubjectRepository;
  private final QualificationRepository qualificationRepository;
  private final ExamScheduleRepository examScheduleRepository;
  private final OpenQuestionRepository openQuestionRepository;
  private final AccountGoalClient accountGoalClient;
  private final ObjectMapper objectMapper;

  @Cacheable(value = "cert-current", key = "#userId", unless = "!#result.isPresent()")
  public Optional<CertInfoDto.CurrentCertResponse> currentCert(String userId) {
    AccountGoalClient.GoalResponse goal = fetchGoal();
    if (goal == null || goal.certId() == null) {
      return Optional.empty();
    }

    CertEntity cert = certRepository.findById(goal.certId()).orElse(null);
    if (cert == null) {
      return Optional.empty();
    }

    QualificationEntity qualification = Optional.ofNullable(cert.getQnetJmCd())
        .flatMap(qualificationRepository::findByJmCd)
        .orElse(null);

    List<CertInfoDto.SubjectSummary> writtenSubjects =
        subjects(cert.getId(), ExamMode.WRITTEN);
    List<CertInfoDto.SubjectSummary> practicalSubjects =
        subjects(cert.getId(), ExamMode.PRACTICAL);

    List<CertInfoDto.ExamScheduleSummary> schedules =
        schedules(cert.getQnetJmCd());

    CertInfoDto.GoalSummary goalSummary = new CertInfoDto.GoalSummary(
        goal.id(),
        goal.userId(),
        goal.certId(),
        goal.targetExamMode(),
        goal.targetRoundId(),
        goal.ddayCached()
    );

    CertInfoDto.CertSummary certSummary = new CertInfoDto.CertSummary(
        cert.getId(),
        cert.getName(),
        cert.getLevel(),
        cert.getIssuer(),
        cert.getIssuerUrl(),
        cert.getDescription(),
        cert.getWrittenFee(),
        cert.getPracticalFee(),
        cert.getPassRuleText(),
        cert.getQnetJmCd()
    );

    CertInfoDto.QualificationSummary qualificationSummary = toQualificationSummary(qualification);

    return Optional.of(new CertInfoDto.CurrentCertResponse(
        goalSummary,
        certSummary,
        qualificationSummary,
        writtenSubjects,
        practicalSubjects,
        schedules
    ));
  }

  @Cacheable(value = "cert-tips", key = "#jmCd", unless = "!#result.isPresent()")
  public Optional<CertInfoDto.TipsResponse> tips(String jmCd) {
    return qualificationRepository.findByJmCd(jmCd)
        .map(qualification -> {
          List<String> keywords = extractKeywords(qualification);
          List<CertInfoDto.OpenQuestionSummary> openQuestions = openQuestions(jmCd);
          String message = buildTipsMessage(qualification, keywords);
          return new CertInfoDto.TipsResponse(
              qualification.getJmCd(),
              toQualificationSummary(qualification),
              keywords,
              openQuestions,
              message
          );
        });
  }

  private AccountGoalClient.GoalResponse fetchGoal() {
    try {
      return accountGoalClient.goal();
    } catch (FeignException e) {
      if (e.status() == 404 || e.status() == 204) {
        return null;
      }
      return null;
    }
  }

  private List<CertInfoDto.SubjectSummary> subjects(Long certId, ExamMode mode) {
    return certSubjectRepository.findByCertIdAndExamModeOrderBySubjectSeqAsc(certId, mode).stream()
        .map(sub -> new CertInfoDto.SubjectSummary(
            sub.getExamMode().name(),
            sub.getName(),
            sub.getTotalQuestions(),
            sub.getDurationMinutes()
        ))
        .toList();
  }

  private CertInfoDto.QualificationSummary toQualificationSummary(QualificationEntity qualification) {
    if (qualification == null) {
      return null;
    }
    return new CertInfoDto.QualificationSummary(
        qualification.getJmCd(),
        qualification.getJmNm(),
        qualification.getEngJmNm(),
        qualification.getSeriesCd(),
        qualification.getSeriesNm(),
        qualification.getImplNm(),
        qualification.getInstiNm(),
        qualification.getSummary(),
        qualification.getJob(),
        qualification.getTrend(),
        qualification.getCareer(),
        qualification.getHist()
    );
  }

  private List<CertInfoDto.ExamScheduleSummary> schedules(String jmCd) {
    if (!StringUtils.hasText(jmCd)) {
      return List.of();
    }
    LocalDate today = LocalDate.now();
    return examScheduleRepository.findTop5BySourceAndJmCdOrderByImplYyDescImplSeqDesc("QNET", jmCd).stream()
        .sorted(Comparator.comparing(
            (ExamScheduleEntity e) -> Optional.ofNullable(e.getDocExamStartDt()).orElse(today))
            .thenComparing(e -> Optional.ofNullable(e.getPracExamStartDt()).orElse(today)))
        .map(e -> new CertInfoDto.ExamScheduleSummary(
            e.getImplYy(),
            e.getImplSeq(),
            e.getQualgbNm(),
            toIso(e.getDocRegStartDt()),
            toIso(e.getDocRegEndDt()),
            toIso(e.getDocExamStartDt()),
            toIso(e.getDocExamEndDt()),
            toIso(e.getDocPassDt()),
            toIso(e.getPracRegStartDt()),
            toIso(e.getPracRegEndDt()),
            toIso(e.getPracExamStartDt()),
            toIso(e.getPracExamEndDt()),
            toIso(e.getPracPassDt())
        ))
        .toList();
  }

  private List<CertInfoDto.OpenQuestionSummary> openQuestions(String jmCd) {
    if (!StringUtils.hasText(jmCd)) {
      return List.of();
    }
    return openQuestionRepository.findTop5ByJmCdOrderByRegDttmDesc(jmCd).stream()
        .map(q -> new CertInfoDto.OpenQuestionSummary(
            q.getArtlSeq(),
            q.getTitle(),
            q.getRegDttm() == null ? null : q.getRegDttm().toString(),
            parseAttachments(q.getAttachmentsJson())
        ))
        .toList();
  }

  private List<CertInfoDto.Attachment> parseAttachments(String json) {
    if (!StringUtils.hasText(json)) {
      return List.of();
    }
    try {
      return objectMapper.readValue(json, ATTACHMENT_TYPE);
    } catch (Exception e) {
      return List.of();
    }
  }

  private String buildTipsMessage(QualificationEntity qualification, List<String> keywords) {
    if (qualification == null) {
      return "큐넷에 등록된 자격정보를 찾을 수 없습니다.";
    }
    if (!keywords.isEmpty()) {
      return "다음 키워드를 중심으로 학습해 보세요: " + String.join(", ", keywords);
    }
    if (StringUtils.hasText(qualification.getSummary())) {
      return qualification.getSummary();
    }
    return "큐넷 자격 정보를 바탕으로 학습 전략을 준비해 보세요.";
  }

  private List<String> extractKeywords(QualificationEntity qualification) {
    if (qualification == null) {
      return List.of();
    }
    String combined = String.join(" ",
        Optional.ofNullable(qualification.getTrend()).orElse(""),
        Optional.ofNullable(qualification.getJob()).orElse(""));
    if (!StringUtils.hasText(combined)) {
      return List.of();
    }
    String[] tokens = combined.split("[,·/\\n]|\\s{2,}");
    return Arrays.stream(tokens)
        .map(String::trim)
        .filter(token -> token.length() > 1)
        .distinct()
        .limit(5)
        .toList();
  }

  private String toIso(LocalDate date) {
    return date == null ? null : date.toString();
  }
}

