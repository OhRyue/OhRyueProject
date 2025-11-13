package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchParticipant;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface MatchParticipantRepository extends JpaRepository<MatchParticipant, Long> {

  List<MatchParticipant> findByRoomId(Long roomId);

  List<MatchParticipant> findByRoomIdIn(Collection<Long> roomIds);

  Optional<MatchParticipant> findByRoomIdAndUserId(Long roomId, String userId);

  long countByRoomId(Long roomId);

  List<MatchParticipant> findByRoomIdAndEliminatedFalse(Long roomId);
}
 