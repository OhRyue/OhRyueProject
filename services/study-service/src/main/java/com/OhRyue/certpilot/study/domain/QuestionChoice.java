// src/main/java/com/OhRyue/certpilot/study/domain/QuestionChoice.java
package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(
    name = "question_choice",
    indexes = {
        @Index(name = "ix_choice_q", columnList = "question_id"),
        @Index(name = "ix_choice_label", columnList = "label")
    }
)
public class QuestionChoice {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "question_id", nullable = false)
  private Long questionId;

  @Column(name = "label", nullable = false, length = 1)
  private String label;   // 'A' ~ 'D'

  @Column(name = "content", nullable = false, length = 1000)
  private String content;

  @Column(name = "is_correct", nullable = false)
  private boolean correct;

  @Column(name = "created_at", nullable = false)
  private java.time.Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = java.time.Instant.now();
    }
  }
}
