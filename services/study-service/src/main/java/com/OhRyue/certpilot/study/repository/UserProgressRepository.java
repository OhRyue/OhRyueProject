package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.UserProgress;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface UserProgressRepository extends JpaRepository<UserProgress, Long> {
  Optional<UserProgress> findByUserIdAndTopicIdAndExamMode(String userId, Long topicId, ExamMode examMode);
  List<UserProgress> findByUserId(String userId);
}
