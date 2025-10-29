package com.OhRyue.certpilot.curriculum.repository;

import com.OhRyue.certpilot.curriculum.entity.ConceptCheck;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ConceptCheckRepository extends JpaRepository<ConceptCheck, Long> {
    List<ConceptCheck> findByConceptId(Long conceptId);
}

