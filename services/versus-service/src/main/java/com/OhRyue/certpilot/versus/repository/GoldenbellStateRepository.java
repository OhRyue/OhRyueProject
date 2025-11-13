package com.OhRyue.certpilot.versus.repository;

import com.OhRyue.certpilot.versus.domain.GoldenbellState;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface GoldenbellStateRepository extends JpaRepository<GoldenbellState, Long> {

  List<GoldenbellState> findByRoomId(Long roomId);

  Optional<GoldenbellState> findByRoomIdAndUserId(Long roomId, String userId);
}
