package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.Concept;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ConceptRepository extends JpaRepository<Concept, Long> {

    Optional<Concept> findByTopicId(Long topicId);
}
