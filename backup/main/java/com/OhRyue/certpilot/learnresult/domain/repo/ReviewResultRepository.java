package com.OhRyue.certpilot.learnresult.domain.repo;

import com.OhRyue.certpilot.learnresult.domain.ReviewResult;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ReviewResultRepository extends JpaRepository<ReviewResult, Long> {
  List<ReviewResult> findByUserIdOrderByCreatedAtDesc(Long userId);
}
