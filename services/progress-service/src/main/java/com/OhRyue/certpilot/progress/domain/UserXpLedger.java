package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity
@Table(name = "user_xp_ledger", indexes = {
    @Index(name="ix_xpledger_user_time", columnList="user_id, created_at")
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserXpLedger {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(nullable=false)
  private int delta;

  @Enumerated(EnumType.STRING)
  @Column(nullable=false, length=16)
  private XpReason reason;

  @Column(length=100)
  private String refId;

  @Column(name="created_at", nullable=false)
  private Instant createdAt;

  @PrePersist void pre() {
    if (createdAt == null) createdAt = Instant.now();
  }
}
