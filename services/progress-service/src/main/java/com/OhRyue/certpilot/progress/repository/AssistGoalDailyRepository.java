package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.AssistGoalDaily;
import com.OhRyue.certpilot.progress.domain.AssistGoalDailyKey;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AssistGoalDailyRepository extends JpaRepository<AssistGoalDaily, AssistGoalDailyKey> {}
