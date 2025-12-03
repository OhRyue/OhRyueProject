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

    /**
     * 사용자의 특정 examMode와 flowType에 해당하는 최근 활동 조회
     * 필기/실기 배지 체크용
     */
    @Query("""
        select a from ProgressActivity a
        where a.userId = :userId
          and a.mode = :examMode
          and a.mainType = :mainType
          and a.activityGroup = 'MAIN'
        order by a.finishedAt desc
    """)
    List<ProgressActivity> findByUserIdAndExamModeAndMainType(
        @Param("userId") String userId,
        @Param("examMode") com.OhRyue.certpilot.progress.domain.enums.ExamMode examMode,
        @Param("mainType") com.OhRyue.certpilot.progress.domain.enums.MainType mainType
    );

    /**
     * 사용자의 정답률 80% 이상 활동 개수 조회
     * ACCURACY_MASTER 배지 체크용
     */
    @Query("""
        select count(a) from ProgressActivity a
        where a.userId = :userId
          and a.accuracyPct >= 80.0
          and a.activityGroup = 'MAIN'
    """)
    long countByUserIdAndAccuracyPctGreaterThanEqual80(String userId);
}



