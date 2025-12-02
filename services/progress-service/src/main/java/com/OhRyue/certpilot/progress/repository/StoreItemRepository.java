package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.StoreItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface StoreItemRepository extends JpaRepository<StoreItem, Long> {
  List<StoreItem> findByIsActiveTrue();
}
