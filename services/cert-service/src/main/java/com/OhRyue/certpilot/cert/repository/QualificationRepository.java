package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface QualificationRepository extends JpaRepository<QualificationEntity, String> {

  List<QualificationEntity> findBySeriesCd(String seriesCd);

  Optional<QualificationEntity> findByJmCd(String jmCd);
}

