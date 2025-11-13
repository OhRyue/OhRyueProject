package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "match_room")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MatchRoom {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 32)
  private MatchMode mode;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private MatchStatus status;

  @Column(name = "scope_json", columnDefinition = "JSON")
  private String scopeJson;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = Instant.now();
    }
    if (status == null) {
      status = MatchStatus.WAIT;
    }
  }
}
 