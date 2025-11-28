package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "learning_session")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LearningSession {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "topic_id", nullable = false)
  private Long topicId;

  @Column(nullable = false, length = 20)
  private String mode; // WRITTEN | PRACTICAL

  @Column(nullable = false, length = 20)
  private String status; // IN_PROGRESS | DONE (과정 완료)

  @Column(name = "truly_completed")
  private Boolean trulyCompleted; // 진정한 완료 (MCQ 완료) - null: 미완료, true: 완료

  @Column(name = "started_at", nullable = false)
  private Instant startedAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @OneToMany(mappedBy = "learningSession", cascade = CascadeType.ALL, orphanRemoval = true)
  @Builder.Default
  private List<LearningStep> steps = new ArrayList<>();

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (startedAt == null) startedAt = now;
    if (updatedAt == null) updatedAt = now;
    if (status == null) status = "IN_PROGRESS";
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}

