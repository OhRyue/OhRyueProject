package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.RankScope;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDate;

@Entity @Table(name="leaderboard_snapshot", indexes = {
    @Index(name="uq_snapshot_scope_date", columnList="scope,snapshot_date", unique=true)
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class LeaderboardSnapshot {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="snapshot_date", nullable=false)
  private LocalDate snapshotDate;

  @Enumerated(EnumType.STRING)
  @Column(nullable=false, length=10)
  private RankScope scope;

  @Column(name="payload_json", columnDefinition="JSON", nullable=false)
  private String payloadJson;
}
