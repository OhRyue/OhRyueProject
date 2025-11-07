package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserProfile;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserProfileRepository extends JpaRepository<UserProfile, String> {
}
