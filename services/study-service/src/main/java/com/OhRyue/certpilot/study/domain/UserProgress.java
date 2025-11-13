package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "user_progress",
    uniqueConstraints = @UniqueConstraint(name = "uq_progress_user_topic", columnNames = {"user_id", "topic_id"}),
    indexes = @Index(name = "ix_progress_updated", columnList = "updated_at")
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserProgress {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "topic_id", nullable = false)
  private Long topicId;

  @Column(name = "written_done_cnt", nullable = false)
  private Integer writtenDoneCnt;

  @Column(name = "practical_done_cnt", nullable = false)
  private Integer practicalDoneCnt;

  @Column(name = "written_accuracy", nullable = false)
  private Double writtenAccuracy;

  @Column(name = "practical_avg_score", nullable = false)
  private Double practicalAvgScore;

  @Column(name = "last_studied_at")
  private Instant lastStudiedAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (updatedAt == null) updatedAt = now;
    if (writtenDoneCnt == null) writtenDoneCnt = 0;
    if (practicalDoneCnt == null) practicalDoneCnt = 0;
    if (writtenAccuracy == null) writtenAccuracy = 0.0;
    if (practicalAvgScore == null) practicalAvgScore = 0.0;
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}
