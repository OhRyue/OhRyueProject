package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "study_session_item",
    indexes = {
        @Index(name = "ix_ssi_session", columnList = "session_id"),
        @Index(name = "ix_ssi_question", columnList = "question_id")
    },
    uniqueConstraints = {
        @UniqueConstraint(name = "uq_ssi_session_order", columnNames = {"session_id", "order_no"}),
        @UniqueConstraint(name = "uq_ssi_session_question", columnNames = {"session_id", "question_id"})
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class StudySessionItem {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "session_id", nullable = false)
  private Long sessionId;

  @Column(name = "order_no", nullable = false)
  private Integer orderNo;

  @Column(name = "question_id", nullable = false)
  private Long questionId;

  @Column(name = "user_answer_json", columnDefinition = "JSON")
  private String userAnswerJson;

  @Column(name = "is_correct")
  private Boolean correct;

  @Column(name = "score")
  private Integer score;

  @Column(name = "answered_at")
  private Instant answeredAt;

  @Column(name = "ai_explain_json", columnDefinition = "JSON")
  private String aiExplainJson;

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) createdAt = Instant.now();
  }
}


