package com.OhRyue.certpilot.learnresult.domain.repo;

import com.OhRyue.certpilot.learnresult.domain.ReviewResultItem;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReviewResultItemRepository extends JpaRepository<ReviewResultItem, Long> {
}
