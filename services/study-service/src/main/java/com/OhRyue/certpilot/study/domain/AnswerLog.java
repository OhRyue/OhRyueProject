package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

/**
 * 사용자 풀이 로그(정/오답, 시각)
 * - 파생쿼리에서 사용되는 필드명: userId, questionId, correct, answeredAt
 */
@Entity
@Table(
    name = "answer_log",
    indexes = {
        @Index(name = "ix_answerlog_user_time", columnList = "user_id,answered_at"),
        @Index(name = "ix_answerlog_question", columnList = "question_id")
    }
)
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AnswerLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** 사용자 식별자(로그인 아이디/UUID 등) */
  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  /** 문제 ID (FK처럼 사용; 실제 FK 제약은 선택) */
  @Column(name = "question_id", nullable = false)
  private Long questionId;

  /** 정답 여부 */
  @Column(nullable = false)
  private Boolean correct;

  /** 풀이 시각(UTC) */
  @Column(name = "answered_at", nullable = false)
  private Instant answeredAt;

  /** 선택지 라벨/텍스트, 제출 답안 등 */
  @Column(length = 50)
  private String userAnswer;     // MCQ/OX라벨 또는 주관식 텍스트 요약

  @Column(length = 20)
  private String mode;           // "WRITTEN" | "PRACTICAL" 등

  @Column(length = 20)
  private String type;           // "OX" | "MCQ" | "SHORT" | "LONG"
}
