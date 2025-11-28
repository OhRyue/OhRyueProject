package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.LearningStep;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface LearningStepRepository extends JpaRepository<LearningStep, Long> {
  List<LearningStep> findByLearningSessionIdOrderByIdAsc(Long learningSessionId);
  Optional<LearningStep> findByLearningSessionIdAndStepCode(Long learningSessionId, String stepCode);
}

