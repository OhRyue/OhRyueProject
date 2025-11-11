package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.LeaderboardSnapshot;
import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.Optional;

public interface LeaderboardSnapshotRepository extends JpaRepository<LeaderboardSnapshot, Long> {
  Optional<LeaderboardSnapshot> findByScopeAndSnapshotDate(RankScope scope, LocalDate date);
}
