package com.OhRyue.certpilot.community.domain;

import com.OhRyue.certpilot.community.domain.enums.ReportStatus;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "post_report",
    indexes = {
        @Index(name = "ix_report_target", columnList = "target_type, target_id"),
        @Index(name = "ix_report_reporter", columnList = "reporter_id")
    })
@Getter
@Setter
public class PostReport {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Enumerated(EnumType.STRING)
  @Column(name = "target_type", nullable = false, length = 16)
  private ReactionTargetType targetType;

  @Column(name = "target_id", nullable = false)
  private Long targetId;

  @Column(name = "reporter_id", nullable = false, length = 100)
  private String reporterId;

  @Column(length = 500)
  private String reason;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private ReportStatus status = ReportStatus.PENDING;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @Column(name = "resolved_at")
  private Instant resolvedAt;

  @PrePersist
  void onCreate() {
    createdAt = Instant.now();
    if (status == null) {
      status = ReportStatus.PENDING;
    }
  }
}

