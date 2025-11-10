package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "user_answer")
@Getter @Setter
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

  @Column(nullable = false)
  private boolean correct;       // 정오 표기 (실기는 score로 PASS/FAIL 판정)

  @Column(nullable = true)
  private Integer score;         // 실기 점수(0~100). 필기는 null 가능

  @Column(name = "answer_text", nullable = true, length = 2000)
  private String answerText;     // 사용자가 입력/선택한 답(라벨/서술)

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @PrePersist
  void prePersist() {
    if (createdAt == null) {
      createdAt = Instant.now();
    }
  }
}
