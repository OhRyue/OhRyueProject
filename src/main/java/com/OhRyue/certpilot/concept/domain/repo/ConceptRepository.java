package com.OhRyue.certpilot.concept.domain.repo;

import com.OhRyue.certpilot.concept.domain.Concept;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ConceptRepository extends JpaRepository<Concept, Long> {
    Page<Concept> findByCertIdAndCategoryContainingIgnoreCase(Long certId, String category, Pageable pageable);
}
