package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "question",
    indexes = {
        @Index(name = "ix_q_topic_mode", columnList = "topic_id, mode"),
        @Index(name = "ix_q_mode_type", columnList = "mode, type"),
        @Index(name = "ix_q_difficulty", columnList = "difficulty"),
        @Index(name = "ix_q_topic_type", columnList = "topic_id, type")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Question {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "cert_id", nullable = false)
  private Long certId;

  @Column(name = "topic_id", nullable = false)
  private Long topicId;

  @Enumerated(EnumType.STRING)
  @Column(name = "mode", nullable = false, length = 16)
  private ExamMode mode;

  @Enumerated(EnumType.STRING)
  @Column(name = "type", nullable = false, length = 16)
  private QuestionType type;

  @Enumerated(EnumType.STRING)
  @Column(name = "difficulty", nullable = false, length = 16)
  private Difficulty difficulty;

  @Lob
  @Column(name = "stem", nullable = false, columnDefinition = "TEXT")
  private String stem;

  @Column(name = "payload_json", columnDefinition = "JSON")
  private String payloadJson;

  @Column(name = "answer_key", columnDefinition = "TEXT")
  private String answerKey;

  @Lob
  @Column(name = "solution_text", columnDefinition = "TEXT")
  private String solutionText;

  @Column(name = "source", length = 100)
  private String source;

  @Column(name = "image_url", length = 500)
  private String imageUrl;

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (createdAt == null) createdAt = now;
    if (updatedAt == null) updatedAt = now;
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}