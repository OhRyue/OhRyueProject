package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.CertEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CertRepository extends JpaRepository<CertEntity, Long> {

  Optional<CertEntity> findByQnetJmCd(String qnetJmCd);
}

