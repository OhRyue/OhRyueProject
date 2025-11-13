package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "goldenbell_state")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GoldenbellState {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "room_id", nullable = false)
  private Long roomId;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "alive", nullable = false)
  private boolean alive;

  @Column(name = "revived", nullable = false)
  private boolean revived;

  public static GoldenbellState alive(Long roomId, String userId) {
    return GoldenbellState.builder()
        .roomId(roomId)
        .userId(userId)
        .alive(true)
        .revived(false)
        .build();
  }
}
 
