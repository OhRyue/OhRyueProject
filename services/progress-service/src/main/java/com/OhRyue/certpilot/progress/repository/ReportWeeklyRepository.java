package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.ReportWeeklyKey;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ReportWeeklyRepository extends JpaRepository<ReportWeekly, ReportWeeklyKey> {
  Optional<ReportWeekly> findByUserIdAndWeekIso(String userId, String weekIso);
  List<ReportWeekly> findByUserId(String userId);
  List<ReportWeekly> findByWeekIsoOrderByXpGainedDesc(String weekIso, Pageable pageable);
  Optional<ReportWeekly> findTopByUserIdOrderByWeekIsoDesc(String userId);
}
