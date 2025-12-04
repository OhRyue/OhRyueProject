package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.QnetQualificationInfoEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface QnetQualificationInfoRepository extends JpaRepository<QnetQualificationInfoEntity, Long> {

    List<QnetQualificationInfoEntity> findByJmCd(String jmCd);

    Optional<QnetQualificationInfoEntity> findByJmCdAndInfogb(String jmCd, String infogb);

    void deleteByJmCd(String jmCd);
}


