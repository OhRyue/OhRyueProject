package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.LearnSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface LearnSessionRepository extends JpaRepository<LearnSession, Long> {
  Optional<LearnSession> findFirstByUserIdAndTopicIdAndModeOrderByIdDesc(String userId, Long topicId, String mode);
  List<LearnSession> findByUserIdOrderByUpdatedAtDesc(String userId);
}
