package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.LearningSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface LearningSessionRepository extends JpaRepository<LearningSession, Long> {
  Optional<LearningSession> findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(String userId, Long topicId, String mode);
  List<LearningSession> findByUserIdOrderByUpdatedAtDesc(String userId);
}

