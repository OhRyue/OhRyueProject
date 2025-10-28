package com.OhRyue.certpilot.learnresult.domain.repo;

import com.OhRyue.certpilot.learnresult.domain.ReviewResultItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ReviewResultItemRepository extends JpaRepository<ReviewResultItem, Long> {
  List<ReviewResultItem> findByResultIdOrderByOrdNoAsc(Long resultId);
}
