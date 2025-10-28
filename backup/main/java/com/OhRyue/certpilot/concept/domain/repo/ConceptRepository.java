package com.OhRyue.certpilot.concept.domain.repo;

import com.OhRyue.certpilot.concept.domain.Concept;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ConceptRepository extends JpaRepository<Concept, Long> {
    Page<Concept> findByCertIdAndCategoryContainingIgnoreCase(Long certId, String category, Pageable pageable);

    // 세세항목에 연결된 최신 개념 1건
    Optional<Concept> findTop1ByTopicIdOrderByIdDesc(Long topicId);
}
