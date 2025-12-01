package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.StudySession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface StudySessionRepository extends JpaRepository<StudySession, Long> {

  Optional<StudySession> findFirstByUserIdAndTopicScopeJsonAndModeOrderByStartedAtDesc(
      String userId, String topicScopeJson, String mode);

  List<StudySession> findByUserIdOrderByStartedAtDesc(String userId);
  
  // MICRO 세션 조회: userId, mode="MICRO", examMode, topic_scope_json에 topicId 포함
  List<StudySession> findByUserIdAndModeAndExamMode(String userId, String mode, com.OhRyue.certpilot.study.domain.enums.ExamMode examMode);
}


