package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDate;

@Entity @Table(name="user_streak")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserStreak {
  @Id
  @Column(length=100)
  private String userId;

  @Column(nullable=false)
  private int currentDays;

  @Column(nullable=false)
  private int bestDays;

  private LocalDate lastActiveDate;
}
