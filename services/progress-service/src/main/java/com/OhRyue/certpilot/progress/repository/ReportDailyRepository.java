package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportDailyKey;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface ReportDailyRepository extends JpaRepository<ReportDaily, ReportDailyKey> {
  Optional<ReportDaily> findByUserIdAndDate(String userId, LocalDate date);
  List<ReportDaily> findByUserId(String userId);
  List<ReportDaily> findByUserIdAndDateBetween(String userId, LocalDate start, LocalDate end);
}
