package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;   // ✅ 추가

public interface MatchEventRepository extends JpaRepository<MatchEvent, Long> {

    List<MatchEvent> findByRoomIdOrderByCreatedAtDesc(Long roomId, Pageable pageable);

    List<MatchEvent> findByRoomIdOrderByCreatedAtAsc(Long roomId);

    List<MatchEvent> findByRoomIdAndEventType(Long roomId, String eventType);

    List<MatchEvent> findByRoomIdAndEventTypeContaining(Long roomId, String eventType);

    // VersusService.recordQuestionStartIfNeeded 에서 사용하는 메서드
    Optional<MatchEvent> findFirstByRoomIdAndEventTypeAndPayloadJsonContainingOrderByCreatedAtDesc(
            Long roomId,
            String eventType,
            String payloadJsonFragment
    );
}
