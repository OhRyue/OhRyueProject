package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "user_answer",
    indexes = {
        @Index(name = "ix_uans_user_time", columnList = "user_id, answered_at"),
        @Index(name = "ix_uans_question_time", columnList = "question_id, answered_at"),
        @Index(name = "ix_uans_user_question_time", columnList = "user_id, question_id, answered_at")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserAnswer {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "question_id", nullable = false)
  private Long questionId;

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", nullable = false, length = 16)
  private ExamMode examMode;

  @Enumerated(EnumType.STRING)
  @Column(name = "question_type", nullable = false, length = 16)
  private QuestionType questionType;

  @Column(name = "answered_at", nullable = false)
  private Instant answeredAt;

  @Column(name = "user_answer_json", columnDefinition = "JSON")
  private String userAnswerJson;

  @Column(name = "is_correct")
  private Boolean correct;

  @Column(name = "score")
  private Integer score;

  @Column(name = "source", length = 30)
  private String source;

  @Column(name = "session_id")
  private Long sessionId;

  @Column(name = "session_item_id")
  private Long sessionItemId;

  @PrePersist
  void onCreate() {
    if (answeredAt == null) {
      answeredAt = Instant.now();
    }
  }
}
