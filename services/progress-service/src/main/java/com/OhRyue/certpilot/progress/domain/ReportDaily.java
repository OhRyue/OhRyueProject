package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;

@Entity @Table(name="report_daily")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
@IdClass(ReportDailyKey.class)
public class ReportDaily {
  @Id @Column(length=100)
  private String userId;

  @Id
  private LocalDate date;

  @Column(nullable=false)
  private int solvedCount;

  @Column(nullable=false)
  private int timeSpentSec;

  @Column(nullable=false, precision = 5, scale = 2)
  private BigDecimal accuracy;

  @Column(nullable=false)
  private int xpGained;
}
