package com.OhRyue.certpilot.learn.domain.repo;

import com.OhRyue.certpilot.learn.domain.LearnSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface LearnSessionRepository extends JpaRepository<LearnSession, Long> {
  List<LearnSession> findTop20ByUserIdOrderByCreatedAtDesc(Long userId);
}
