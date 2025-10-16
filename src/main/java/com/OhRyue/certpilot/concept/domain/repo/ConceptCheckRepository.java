package com.OhRyue.certpilot.concept.domain.repo;

import com.OhRyue.certpilot.concept.domain.ConceptCheck;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ConceptCheckRepository extends JpaRepository<ConceptCheck, Long> {
    List<ConceptCheck> findByConceptId(Long conceptId);
}

