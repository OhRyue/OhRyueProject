package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserGoalCert;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserGoalCertRepository extends JpaRepository<UserGoalCert, Long> {
  Optional<UserGoalCert> findByUserId(String userId);
}
