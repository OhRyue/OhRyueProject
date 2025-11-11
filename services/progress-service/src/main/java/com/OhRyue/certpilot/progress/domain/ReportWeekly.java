package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity @Table(name="report_weekly")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
@IdClass(ReportWeeklyKey.class)
public class ReportWeekly {
  @Id @Column(length=100)
  private String userId;

  @Id @Column(length=10)
  private String weekIso;

  @Column(nullable=false)
  private int solvedCount;

  @Column(nullable=false)
  private int timeSpentSec;

  @Column(nullable=false, precision = 5, scale = 2)
  private BigDecimal accuracy;

  @Column(nullable=false)
  private int xpGained;
}
