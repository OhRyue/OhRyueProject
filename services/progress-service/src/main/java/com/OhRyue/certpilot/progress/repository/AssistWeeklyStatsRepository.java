package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.AssistWeeklyStats;
import com.OhRyue.certpilot.progress.domain.AssistWeeklyKey;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AssistWeeklyStatsRepository extends JpaRepository<AssistWeeklyStats, AssistWeeklyKey> {}
