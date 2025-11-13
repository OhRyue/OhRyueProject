package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface MatchEventRepository extends JpaRepository<MatchEvent, Long> {

  List<MatchEvent> findByRoomIdOrderByCreatedAtDesc(Long roomId, Pageable pageable);

  List<MatchEvent> findByRoomIdOrderByCreatedAtAsc(Long roomId);
}

