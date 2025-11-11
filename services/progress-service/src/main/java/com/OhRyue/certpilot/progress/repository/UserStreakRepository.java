package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserStreakRepository extends JpaRepository<UserStreak, String> {}
