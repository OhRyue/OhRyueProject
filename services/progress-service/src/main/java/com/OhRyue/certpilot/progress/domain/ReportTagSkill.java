package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;

@Entity @Table(name="report_tag_skill", indexes = {
    @Index(name="ix_rtag_user", columnList="user_id"),
    @Index(name="ix_rtag_tag", columnList="tag")
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class ReportTagSkill {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(length=100, nullable=false)
  private String tag;

  @Column(length=10, nullable=false)
  private String examMode; // WRITTEN / PRACTICAL

  @Column(nullable=false)
  private int correct;

  @Column(nullable=false)
  private int total;

  @Column(nullable=false, precision=5, scale=2)
  private BigDecimal accuracy;

  @Column(nullable=false)
  private Instant updatedAt;

  @PrePersist @PreUpdate
  void touch(){ updatedAt = Instant.now(); }
}
