package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.UserProfile;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserProfileRepository extends JpaRepository<UserProfile, String> {
  boolean existsByNickname(String nickname);
  Optional<UserProfile> findByNickname(String nickname);
}
