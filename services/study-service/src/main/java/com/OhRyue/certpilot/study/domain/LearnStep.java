package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "learn_step")
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class LearnStep {

  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "session_id", nullable = false)
  private Long sessionId;

  @Column(nullable = false, length = 30)
  private String step;  // CONCEPT | MINI | REVIEW_WRONG | MCQ | REVIEW_WRONG2

  @Column(nullable = false, length = 20)
  private String state; // READY | PASS | FAIL

  private Integer score; // 정답률(%) 등

  @Lob
  @Column(name = "details_json")
  private String detailsJson;

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) createdAt = Instant.now();
  }
}
