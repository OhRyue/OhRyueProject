package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface OpenQuestionRepository extends JpaRepository<OpenQuestionEntity, Long> {

  Page<OpenQuestionEntity> findByJmCd(String jmCd, Pageable pageable);

  java.util.List<OpenQuestionEntity> findTop5ByJmCdOrderByRegDttmDesc(String jmCd);
}

