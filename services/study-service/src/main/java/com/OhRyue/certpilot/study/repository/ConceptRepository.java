package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.Concept;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ConceptRepository extends JpaRepository<Concept, Long> {
  Optional<Concept> findByTopicId(Long topicId);
}
