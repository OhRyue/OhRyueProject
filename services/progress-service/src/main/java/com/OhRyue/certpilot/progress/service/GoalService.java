package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.AssistGoalDaily;
import com.OhRyue.certpilot.progress.domain.AssistGoalDailyKey;
import com.OhRyue.certpilot.progress.repository.AssistGoalDailyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.ZoneId;

@Service @RequiredArgsConstructor
public class GoalService {
  private static final ZoneId KST = ZoneId.of("Asia/Seoul");
  private final AssistGoalDailyRepository repo;

  @Transactional(readOnly = true)
  public AssistGoalDaily getToday(String userId){
    LocalDate today = LocalDate.now(KST);
    return repo.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId).date(today).targetCount(0).progressCount(0).build());
  }

  @Transactional
  public AssistGoalDaily setTarget(String userId, int target){
    LocalDate today = LocalDate.now(KST);
    AssistGoalDaily g = repo.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId).date(today).targetCount(0).progressCount(0).build());
    g.setTargetCount(target);
    return repo.save(g);
  }

  @Transactional
  public AssistGoalDaily increment(String userId, int inc){
    LocalDate today = LocalDate.now(KST);
    AssistGoalDaily g = repo.findById(new AssistGoalDailyKey(userId, today))
        .orElse(AssistGoalDaily.builder()
            .userId(userId).date(today).targetCount(0).progressCount(0).build());
    g.setProgressCount(Math.max(0, g.getProgressCount()+inc));
    return repo.save(g);
  }
}
