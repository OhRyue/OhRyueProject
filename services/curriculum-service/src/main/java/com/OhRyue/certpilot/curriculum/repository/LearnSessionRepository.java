package com.OhRyue.certpilot.curriculum.repository;

import com.OhRyue.certpilot.curriculum.entity.LearnSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface LearnSessionRepository extends JpaRepository<LearnSession, Long> {
  List<LearnSession> findTop20ByUserIdOrderByCreatedAtDesc(Long userId);
}

