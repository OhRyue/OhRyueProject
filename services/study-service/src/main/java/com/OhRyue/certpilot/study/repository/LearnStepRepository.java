package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.LearnStep;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface LearnStepRepository extends JpaRepository<LearnStep, Long> {
  List<LearnStep> findBySessionIdOrderByIdAsc(Long sessionId);
}
