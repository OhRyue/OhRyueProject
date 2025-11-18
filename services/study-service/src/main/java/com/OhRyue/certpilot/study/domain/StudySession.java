package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "study_session",
    indexes = {
        @Index(name = "ix_ss_user_status", columnList = "user_id, status"),
        @Index(name = "ix_ss_user_started", columnList = "user_id, started_at"),
        @Index(name = "ix_ss_mode", columnList = "mode, exam_mode")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class StudySession {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "mode", nullable = false, length = 32)
  private String mode; // MICRO, REVIEW, ASSIST_CATEGORY, ...

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", nullable = false, length = 16)
  private ExamMode examMode;

  @Column(name = "topic_scope_json", columnDefinition = "JSON")
  private String topicScopeJson;

  @Column(name = "question_count", nullable = false)
  private Integer questionCount;

  @Column(name = "started_at", nullable = false)
  private Instant startedAt;

  @Column(name = "finished_at")
  private Instant finishedAt;

  @Column(name = "score_pct")
  private Double scorePct;

  @Column(name = "summary_json", columnDefinition = "JSON")
  private String summaryJson;

  @Column(name = "status", nullable = false, length = 16)
  private String status; // OPEN, SUBMITTED, CLOSED

  @Column(name = "completed")
  private Boolean completed; // 모든 문제를 풀었는지

  @Column(name = "passed")
  private Boolean passed; // 모든 문제를 맞췄는지

  @Column(name = "xp_granted")
  private Boolean xpGranted; // XP가 이미 반영되었는지

  @PrePersist
  void onCreate() {
    if (startedAt == null) startedAt = Instant.now();
    if (status == null) status = "OPEN";
    if (completed == null) completed = false;
    if (passed == null) passed = false;
    if (xpGranted == null) xpGranted = false;
  }
}


