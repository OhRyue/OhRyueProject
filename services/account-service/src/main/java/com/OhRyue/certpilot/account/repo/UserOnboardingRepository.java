package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserOnboarding;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserOnboardingRepository extends JpaRepository<UserOnboarding, String> {
}

