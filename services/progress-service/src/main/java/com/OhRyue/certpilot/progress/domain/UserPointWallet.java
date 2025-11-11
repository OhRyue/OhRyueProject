package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity @Table(name="user_point_wallet")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserPointWallet {
  @Id @Column(length=100)
  private String userId;

  @Column(nullable=false)
  private long pointTotal;

  @Column(nullable=false)
  private Instant updatedAt;

  @PrePersist @PreUpdate
  void touch(){ updatedAt = Instant.now(); }
}
