package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.BattleRecord;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;

public interface BattleRecordRepository extends JpaRepository<BattleRecord, Long> {
    
    List<BattleRecord> findByUserIdOrderByCompletedAtDesc(String userId);
    
    @Query("SELECT br FROM BattleRecord br WHERE br.userId = :userId ORDER BY br.completedAt DESC")
    List<BattleRecord> findRecentByUser(@Param("userId") String userId, org.springframework.data.domain.Pageable pageable);
    
    List<BattleRecord> findByUserIdAndCompletedAtBetweenOrderByCompletedAtDesc(
        String userId, Instant from, Instant to);
}

