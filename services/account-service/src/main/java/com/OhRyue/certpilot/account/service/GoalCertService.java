package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.repo.UserGoalCertRepository;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
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

  public UserGoalCert updateTargetExamDate(String userId, LocalDate targetExamDate) {
    return goalRepo.findByUserId(userId)
        .map(exist -> {
          exist.setTargetExamDate(targetExamDate);
          return goalRepo.save(exist);
        })
        .orElseThrow(() -> new IllegalStateException("목표 자격증이 설정되지 않았습니다. 먼저 목표를 설정해주세요."));
  }
}
