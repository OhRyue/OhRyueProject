package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;
import java.time.LocalDate;

@Entity @Table(name="assist_goal_daily")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
@IdClass(AssistGoalDailyKey.class)
public class AssistGoalDaily {
  @Id @Column(length=100)
  private String userId;

  @Id
  private LocalDate date;

  @Column(nullable=false)
  private int targetCount;

  @Column(nullable=false)
  private int progressCount;

  @Column(nullable=false)
  private Instant updatedAt;

  @PrePersist @PreUpdate
  void touch(){ updatedAt = Instant.now(); }
}
