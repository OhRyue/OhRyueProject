package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.AssistGoalDaily;
import com.OhRyue.certpilot.progress.domain.AssistGoalDailyKey;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;

public interface AssistGoalDailyRepository extends JpaRepository<AssistGoalDaily, AssistGoalDailyKey> {

  List<AssistGoalDaily> findByUserIdAndDateBetween(String userId, LocalDate start, LocalDate end);
}
