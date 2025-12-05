package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;

public interface MatchRoomRepository extends JpaRepository<MatchRoom, Long> {

  List<MatchRoom> findByMode(MatchMode mode);

  List<MatchRoom> findByStatus(MatchStatus status);

  List<MatchRoom> findByModeAndStatus(MatchMode mode, MatchStatus status);

  @Query("SELECT r FROM MatchRoom r WHERE r.mode = :mode AND r.status = :status AND r.scheduledAt <= :now")
  List<MatchRoom> findByModeAndStatusAndScheduledAtLessThanEqual(
      @Param("mode") MatchMode mode,
      @Param("status") MatchStatus status,
      @Param("now") Instant now
  );

  @Query("SELECT r FROM MatchRoom r WHERE r.mode = :mode AND r.status = 'WAIT' AND r.scheduledAt IS NOT NULL AND r.scheduledAt > :now ORDER BY r.scheduledAt ASC")
  List<MatchRoom> findScheduledRoomsByMode(
      @Param("mode") MatchMode mode,
      @Param("now") Instant now
  );
}
 
