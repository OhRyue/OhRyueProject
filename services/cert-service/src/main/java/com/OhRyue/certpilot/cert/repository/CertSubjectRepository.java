package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.CertSubjectEntity;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CertSubjectRepository extends JpaRepository<CertSubjectEntity, Long> {

  List<CertSubjectEntity> findByCertIdOrderBySubjectSeqAsc(Long certId);

  List<CertSubjectEntity> findByCertIdAndExamModeOrderBySubjectSeqAsc(Long certId, ExamMode examMode);
}

