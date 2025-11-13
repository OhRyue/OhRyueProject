package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserStreak;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserStreakRepository extends JpaRepository<UserStreak, String> {

  List<UserStreak> findTop10ByOrderByBestDaysDesc();
}
