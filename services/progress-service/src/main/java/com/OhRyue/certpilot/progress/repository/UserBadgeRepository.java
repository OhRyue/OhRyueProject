package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserBadge;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserBadgeRepository extends JpaRepository<UserBadge, Long> {

  List<UserBadge> findByUserIdOrderByEarnedAtDesc(String userId);

  boolean existsByUserIdAndBadgeId(String userId, Long badgeId);
}

