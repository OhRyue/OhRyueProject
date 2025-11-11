package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity @Table(name="user_badge", indexes = {
    @Index(name="ix_ub_user", columnList="user_id"),
    @Index(name="ix_ub_badge", columnList="badge_id"),
    @Index(name="uq_user_badge", columnList="user_id,badge_id", unique = true)
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserBadge {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(name="badge_id", nullable=false)
  private Long badgeId;

  @Column(name="earned_at", nullable=false)
  private Instant earnedAt;

  @PrePersist void pre(){ if(earnedAt==null) earnedAt=Instant.now(); }
}
