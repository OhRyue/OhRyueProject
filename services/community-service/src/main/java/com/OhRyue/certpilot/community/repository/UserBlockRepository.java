package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.UserBlock;
import com.OhRyue.certpilot.community.domain.UserBlock.UserBlockId;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserBlockRepository extends JpaRepository<UserBlock, UserBlockId> {

  List<UserBlock> findByUserId(String userId);

  boolean existsByUserIdAndBlockedUserId(String userId, String blockedUserId);

  void deleteByUserIdAndBlockedUserId(String userId, String blockedUserId);
}

