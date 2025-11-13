package com.OhRyue.certpilot.community.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "reaction")
@Getter
@Setter
public class Reaction {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Enumerated(EnumType.STRING)
  @Column(name = "target_type", nullable = false, length = 16)
  private ReactionTargetType targetType;

  @Column(name = "target_id", nullable = false)
  private Long targetId;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = Instant.now();
    }
  }
}


