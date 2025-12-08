package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserSkillCounter;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserSkillCounterRepository extends JpaRepository<UserSkillCounter, String> {
  
  Optional<UserSkillCounter> findByUserId(String userId);
}






