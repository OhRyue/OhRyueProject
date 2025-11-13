package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserRankScore;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserRankScoreRepository extends JpaRepository<UserRankScore, String> {
  List<UserRankScore> findTop10ByOrderByScoreDesc();

  long countByScoreGreaterThan(long score);

  List<UserRankScore> findAllByOrderByScoreDesc(Pageable pageable);

  List<UserRankScore> findByUserIdInOrderByScoreDesc(List<String> userIds);
}
