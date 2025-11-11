package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.StoreItem;
import com.OhRyue.certpilot.progress.domain.enums.ItemCategory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface StoreItemRepository extends JpaRepository<StoreItem, Long> {
  List<StoreItem> findByIsActiveTrue();
  List<StoreItem> findByIsActiveTrueAndCategory(ItemCategory category);
}
