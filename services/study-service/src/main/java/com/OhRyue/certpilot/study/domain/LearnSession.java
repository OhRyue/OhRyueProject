package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "learn_session")
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class LearnSession {

  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "topic_id", nullable = false)
  private Long topicId;

  @Column(nullable = false, length = 20)
  private String mode; // WRITTEN | PRACTICAL

  @Column(nullable = false, length = 20)
  private String status; // IN_PROGRESS | DONE

  @Lob
  @Column(name = "progress_json")
  private String progressJson; // 임시 문자열(JSON)

  @Column(name = "started_at", nullable = false)
  private Instant startedAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (startedAt == null) startedAt = now;
    if (updatedAt == null) updatedAt = now;
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}
