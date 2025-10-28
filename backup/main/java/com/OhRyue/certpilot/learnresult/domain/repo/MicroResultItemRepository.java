package com.OhRyue.certpilot.learnresult.domain.repo;

import com.OhRyue.certpilot.learnresult.domain.MicroResultItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface MicroResultItemRepository extends JpaRepository<MicroResultItem, Long> {
  List<MicroResultItem> findByResultIdOrderByOrdNoAsc(Long resultId);
}
