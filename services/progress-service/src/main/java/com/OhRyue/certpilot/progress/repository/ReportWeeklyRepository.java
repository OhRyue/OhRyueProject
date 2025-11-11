package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.ReportWeeklyKey;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReportWeeklyRepository extends JpaRepository<ReportWeekly, ReportWeeklyKey> {}
