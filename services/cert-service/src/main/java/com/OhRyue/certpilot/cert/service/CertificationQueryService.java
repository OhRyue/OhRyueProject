package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.repository.ExamScheduleRepository;
import com.OhRyue.certpilot.cert.repository.OpenQuestionRepository;
import com.OhRyue.certpilot.cert.repository.QualificationRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CertificationQueryService {

  private final QualificationRepository qualificationRepository;
  private final ExamScheduleRepository examScheduleRepository;
  private final OpenQuestionRepository openQuestionRepository;

  public List<QualificationEntity> findQualifications(String seriesCd) {
    if (StringUtils.hasText(seriesCd)) {
      return qualificationRepository.findBySeriesCd(seriesCd);
    }
    return qualificationRepository.findAll();
  }

  public List<ExamScheduleEntity> findExamSchedules(String implYy, String qualgbCd, String jmCd) {
    return examScheduleRepository.search(implYy, qualgbCd, jmCd);
  }

  public Page<OpenQuestionEntity> findOpenQuestions(String jmCd, int page, int size) {
    PageRequest pageable = PageRequest.of(Math.max(0, page - 1), Math.min(size, 100));
    if (StringUtils.hasText(jmCd)) {
      return openQuestionRepository.findByJmCd(jmCd, pageable);
    }
    return openQuestionRepository.findAll(pageable);
  }
}

