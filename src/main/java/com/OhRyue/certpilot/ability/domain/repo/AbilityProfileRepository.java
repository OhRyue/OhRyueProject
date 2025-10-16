package com.OhRyue.certpilot.ability.domain.repo;

import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.AbilityProfileId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AbilityProfileRepository extends JpaRepository<AbilityProfile, AbilityProfileId> {
  List<AbilityProfile> findTop10ByUserIdOrderByUpdatedAtDesc(Long userId);
}
