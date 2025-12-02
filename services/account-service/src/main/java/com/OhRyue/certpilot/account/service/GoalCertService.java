package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.repo.UserGoalCertRepository;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;

@Service
public class GoalCertService {

  private final UserGoalCertRepository goalRepo;

  public GoalCertService(UserGoalCertRepository goalRepo) {
    this.goalRepo = goalRepo;
  }

  public Optional<UserGoalCert> getByUser(String userId) {
    return goalRepo.findByUserId(userId);
  }

  public UserGoalCert upsert(UserGoalCert req) {
    // 사용자당 1개 UNIQUE: 있으면 업데이트, 없으면 생성
    return goalRepo.findByUserId(req.getUserId())
        .map(exist -> {
          exist.setCertId(req.getCertId());
          exist.setTargetExamMode(req.getTargetExamMode());
          exist.setTargetRoundId(req.getTargetRoundId());
          exist.setTargetExamDate(req.getTargetExamDate());
          // dday_cached는 외부(cert-service 스케줄) 동기화 시 갱신
          return goalRepo.save(exist);
        })
        .orElseGet(() -> {
          req.setCreatedAt(LocalDateTime.now());
          return goalRepo.save(req);
        });
  }
}
