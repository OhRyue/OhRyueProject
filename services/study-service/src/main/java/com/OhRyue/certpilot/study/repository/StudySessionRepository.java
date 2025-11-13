package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.StudySession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface StudySessionRepository extends JpaRepository<StudySession, Long> {

  Optional<StudySession> findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(
      String userId, String topicScopeJson, String mode);

  List<StudySession> findByUserIdOrderByStartedAtDesc(String userId);
}


