package com.OhRyue.certpilot.learnresult.domain.repo;

import com.OhRyue.certpilot.learnresult.domain.MicroResult;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface MicroResultRepository extends JpaRepository<MicroResult, Long> {
  List<MicroResult> findByUserIdOrderByCreatedAtDesc(Long userId);
}
