package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface MatchEventRepository extends JpaRepository<MatchEvent, Long> {

  List<MatchEvent> findByRoomIdOrderByCreatedAtDesc(Long roomId, Pageable pageable);

  List<MatchEvent> findByRoomIdOrderByCreatedAtAsc(Long roomId);

  List<MatchEvent> findByRoomIdAndEventType(Long roomId, String eventType);

  List<MatchEvent> findByRoomIdAndEventTypeContaining(Long roomId, String eventType);

  Optional<MatchEvent> findFirstByRoomIdAndEventTypeAndPayloadJsonContainingOrderByCreatedAtDesc(
      Long roomId,
      String eventType,
      String payloadJsonFragment
  );

  boolean existsByRoomIdAndEventTypeAndPayloadJsonContaining(
      Long roomId,
      String eventType,
      String payloadJsonFragment
  );
}
