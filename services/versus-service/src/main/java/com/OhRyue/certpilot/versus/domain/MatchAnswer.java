package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "match_answer")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MatchAnswer {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "room_id", nullable = false)
  private Long roomId;

  @Column(name = "round_no")
  private Integer roundNo;

  @Enumerated(EnumType.STRING)
  @Column(name = "phase", length = 16)
  private MatchPhase phase;

  @Column(name = "question_id", nullable = false)
  private Long questionId;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "submitted_at", nullable = false, updatable = false)
  private Instant submittedAt;

  @Column(name = "is_correct", nullable = false)
  private boolean correct;

  @Column(name = "time_ms", nullable = false)
  private Integer timeMs;

  @Column(name = "score_delta", nullable = false)
  private Integer scoreDelta;

  @Column(name = "user_answer", columnDefinition = "TEXT")
  private String userAnswer;

  @PrePersist
  void onSubmit() {
    if (submittedAt == null) {
      submittedAt = Instant.now();
    }
    if (phase == null) {
      phase = MatchPhase.MAIN;
    }
    if (timeMs == null) {
      timeMs = 0;
    }
    if (scoreDelta == null) {
      scoreDelta = 0;
    }
  }
}
 