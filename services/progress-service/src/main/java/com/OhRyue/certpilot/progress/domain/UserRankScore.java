package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity @Table(name="user_rank_score")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserRankScore {
  @Id
  @Column(length=100)
  private String userId;

  @Column(nullable=false)
  private long score;

  @Column(nullable=false)
  private Instant lastUpdatedAt;

  @PrePersist void pre(){ if(lastUpdatedAt==null) lastUpdatedAt=Instant.now(); }
}
