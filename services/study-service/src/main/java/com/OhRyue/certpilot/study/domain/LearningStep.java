package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "learning_step",
    indexes = {
        @Index(name = "ix_ls_learning_session", columnList = "learning_session_id"),
        @Index(name = "ix_ls_step_code", columnList = "learning_session_id, step_code")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LearningStep {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "learning_session_id", nullable = false)
  private LearningSession learningSession;

  @Column(name = "step_code", nullable = false, length = 30)
  private String stepCode;  // CONCEPT | MINI | REVIEW_WRONG | MCQ | PRACTICAL | REVIEW_WRONG2 | SUMMARY

  @Column(nullable = false, length = 20)
  private String status; // READY | IN_PROGRESS | COMPLETE

  @Column(name = "score_pct")
  private Integer scorePct; // 정답률(%) 등

  @Lob
  @Column(name = "metadata_json", columnDefinition = "JSON")
  private String metadataJson; // 단계별 메타데이터 (JSON)

  @OneToOne(fetch = FetchType.LAZY, mappedBy = "learningStep")
  private StudySession studySession; // 해당 단계의 문제 풀이 세션

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (createdAt == null) createdAt = now;
    if (updatedAt == null) updatedAt = now;
    if (status == null) status = "READY";
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}

