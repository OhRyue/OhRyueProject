package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.TournamentBracket;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface TournamentBracketRepository extends JpaRepository<TournamentBracket, Long> {

  List<TournamentBracket> findByRoomId(Long roomId);

  List<TournamentBracket> findByRoomIdOrderByRoundNoAsc(Long roomId);

  Optional<TournamentBracket> findByRoomIdAndRoundNo(Long roomId, Integer roundNo);
}
 
