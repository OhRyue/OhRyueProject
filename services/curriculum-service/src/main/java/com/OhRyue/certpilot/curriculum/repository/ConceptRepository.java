package com.OhRyue.certpilot.curriculum.repository;

import com.OhRyue.certpilot.curriculum.entity.Concept;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ConceptRepository extends JpaRepository<Concept, Long> {
    Page<Concept> findByCertIdAndCategoryContainingIgnoreCase(Long certId, String category, Pageable pageable);
    Optional<Concept> findTop1ByTopicIdOrderByIdDesc(Long topicId);
}

