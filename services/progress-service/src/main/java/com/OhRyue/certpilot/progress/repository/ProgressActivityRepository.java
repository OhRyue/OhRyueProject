package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.ProgressActivity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface ProgressActivityRepository extends JpaRepository<ProgressActivity, Long> {

    List<ProgressActivity> findTop5ByUserIdOrderByFinishedAtDesc(String userId);

    Page<ProgressActivity> findByUserIdOrderByFinishedAtDesc(String userId, Pageable pageable);

    @Query("""
        select a from ProgressActivity a
        where a.userId = :userId
          and a.finishedAt between :start and :end
        order by a.finishedAt desc
    """)
    List<ProgressActivity> findByUserIdAndFinishedAtBetween(
        @Param("userId") String userId,
        @Param("start") LocalDateTime start,
        @Param("end") LocalDateTime end
    );

    @Query("""
        select a from ProgressActivity a
        where a.userId = :userId
          and a.sourceService = :sourceService
          and a.sourceSessionId = :sourceSessionId
    """)
    List<ProgressActivity> findBySource(
        @Param("userId") String userId,
        @Param("sourceService") String sourceService,
        @Param("sourceSessionId") Long sourceSessionId
    );
}



