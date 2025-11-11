package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;

@Entity @Table(name="assist_weekly_stats")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
@IdClass(AssistWeeklyKey.class)
public class AssistWeeklyStats {
  @Id @Column(length=100)
  private String userId;

  @Id @Column(length=10)
  private String weekIso;

  @Column(nullable=false)
  private int solvedCount;

  @Column(nullable=false, precision = 5, scale = 2)
  private BigDecimal avgAccuracy;
}
