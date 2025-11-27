package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "match_participant")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MatchParticipant {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "room_id", nullable = false)
  private Long roomId;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "joined_at", nullable = false, updatable = false)
  private Instant joinedAt;

  @Column(name = "final_score")
  private Integer finalScore;

  @Column(name = "player_rank")
  private Integer playerRank;

  @Column(name = "eliminated", nullable = false)
  private boolean eliminated;

  @PrePersist
  void onJoin() {
    if (joinedAt == null) {
      joinedAt = Instant.now();
    }
    this.eliminated = false;
  }
}
 