package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchMode;
import com.OhRyue.certpilot.versus.domain.MatchRoom;
import com.OhRyue.certpilot.versus.domain.MatchStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface MatchRoomRepository extends JpaRepository<MatchRoom, Long> {

  List<MatchRoom> findByMode(MatchMode mode);

  List<MatchRoom> findByStatus(MatchStatus status);

  List<MatchRoom> findByModeAndStatus(MatchMode mode, MatchStatus status);
}
 
