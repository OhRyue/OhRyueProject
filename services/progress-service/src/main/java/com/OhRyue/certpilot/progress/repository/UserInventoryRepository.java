package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserInventory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserInventoryRepository extends JpaRepository<UserInventory, Long> {
  boolean existsByUserIdAndItemId(String userId, Long itemId);
  long countByUserIdAndItemId(String userId, Long itemId);
  List<UserInventory> findByUserId(String userId);
}
