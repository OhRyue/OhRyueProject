package com.OhRyue.certpilot.ability.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "ability_profile")
@IdClass(AbilityProfileId.class)
public class AbilityProfile {

  @Id
  @Column(name = "user_id", nullable = false)
  private Long userId;

  @Id
  @Column(name = "tag", nullable = false, length = 100)
  private String tag;

  @Column(name = "attempts", nullable = false)
  private Integer attempts = 0;

  @Column(name = "corrects", nullable = false)
  private Integer corrects = 0;

  @Column(name = "ema_correct", nullable = false, precision = 5, scale = 4)
  private BigDecimal emaCorrect = new BigDecimal("0.5000");

  @Column(name = "updated_at", insertable = false, updatable = false)
  private Instant updatedAt;

  public AbilityProfile(Long userId, String tag) {
    this.userId = userId;
    this.tag = tag;
    this.attempts = 0;
    this.corrects = 0;
    this.emaCorrect = new BigDecimal("0.5000");
  }

  public void applyResult(boolean correct, double alpha) {
    attempts++;
    if (correct) corrects++;

    double prev = emaCorrect.doubleValue();
    double cur = correct ? 1.0 : 0.0;
    double ema = alpha * cur + (1.0 - alpha) * prev;

    this.emaCorrect = BigDecimal.valueOf(ema).setScale(4, RoundingMode.HALF_UP);
  }
}

