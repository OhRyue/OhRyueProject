package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.UserProgress;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface UserProgressRepository extends JpaRepository<UserProgress, Long> {
  Optional<UserProgress> findByUserIdAndTopicId(String userId, Long topicId);
  List<UserProgress> findByUserId(String userId);
}
