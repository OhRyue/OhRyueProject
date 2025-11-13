package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.StudySessionItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface StudySessionItemRepository extends JpaRepository<StudySessionItem, Long> {

  List<StudySessionItem> findBySessionIdOrderByOrderNoAsc(Long sessionId);

  Optional<StudySessionItem> findBySessionIdAndQuestionId(Long sessionId, Long questionId);
}


