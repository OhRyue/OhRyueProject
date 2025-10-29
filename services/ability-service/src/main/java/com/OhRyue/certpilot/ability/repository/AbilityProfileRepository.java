package com.OhRyue.certpilot.ability.repository;

import com.OhRyue.certpilot.ability.entity.AbilityProfile;
import com.OhRyue.certpilot.ability.entity.AbilityProfileId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface AbilityProfileRepository extends JpaRepository<AbilityProfile, AbilityProfileId> {
  List<AbilityProfile> findByUserIdOrderByEmaCorrectAsc(Long userId);
}

