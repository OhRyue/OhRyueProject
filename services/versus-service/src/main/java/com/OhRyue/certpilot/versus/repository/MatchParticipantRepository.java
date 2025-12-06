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

  @Query("SELECT p.roomId FROM MatchParticipant p " +
         "JOIN MatchRoom r ON p.roomId = r.id " +
         "WHERE p.userId = :userId AND r.status = :status AND r.mode = :mode " +
         "ORDER BY r.createdAt DESC")
  List<Long> findActiveRoomIdsByUserIdAndMode(@Param("userId") String userId, @Param("status") MatchStatus status, @Param("mode") com.OhRyue.certpilot.versus.domain.MatchMode mode);

  @Query("SELECT p.roomId FROM MatchParticipant p " +
         "JOIN MatchRoom r ON p.roomId = r.id " +
         "WHERE p.userId = :userId AND r.status = :status AND r.mode = :mode AND r.isBotMatch = false " +
         "ORDER BY r.createdAt DESC")
  List<Long> findActiveNonBotRoomIdsByUserIdAndMode(@Param("userId") String userId, @Param("status") MatchStatus status, @Param("mode") com.OhRyue.certpilot.versus.domain.MatchMode mode);

  @Query("SELECT p.roomId FROM MatchParticipant p " +
         "JOIN MatchRoom r ON p.roomId = r.id " +
         "WHERE p.userId = :userId AND r.status = :status AND r.isBotMatch = false " +
         "ORDER BY r.createdAt DESC")
  List<Long> findActiveNonBotRoomIdsByUserId(@Param("userId") String userId, @Param("status") MatchStatus status);

  /**
   * 하트비트 타임아웃된 참가자 조회
   * - WAIT 상태의 모든 방
   * - ONGOING 상태의 DUEL 방
   * lastHeartbeatAt이 thresholdTime 이전인 참가자들을 조회
   */
  @Query("SELECT p FROM MatchParticipant p " +
         "JOIN MatchRoom r ON p.roomId = r.id " +
         "WHERE (r.status = 'WAIT' OR (r.status = 'ONGOING' AND r.mode = 'DUEL')) " +
         "AND p.lastHeartbeatAt IS NOT NULL " +
         "AND p.lastHeartbeatAt < :thresholdTime")
  List<MatchParticipant> findTimeoutParticipants(@Param("thresholdTime") java.time.Instant thresholdTime);
}
 