package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchParticipant;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface MatchParticipantRepository extends JpaRepository<MatchParticipant, Long> {

  List<MatchParticipant> findByRoomId(Long roomId);

  List<MatchParticipant> findByRoomIdIn(Collection<Long> roomIds);

  Optional<MatchParticipant> findByRoomIdAndUserId(Long roomId, String userId);

  long countByRoomId(Long roomId);

  List<MatchParticipant> findByRoomIdAndEliminatedFalse(Long roomId);

  @Query("SELECT p.roomId FROM MatchParticipant p " +
         "JOIN MatchRoom r ON p.roomId = r.id " +
         "WHERE p.userId = :userId AND r.status = :status " +
         "ORDER BY r.createdAt DESC")
  List<Long> findActiveRoomIdsByUserId(@Param("userId") String userId, @Param("status") MatchStatus status);
}
 