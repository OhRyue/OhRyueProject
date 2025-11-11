package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportDailyKey;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReportDailyRepository extends JpaRepository<ReportDaily, ReportDailyKey> {}
