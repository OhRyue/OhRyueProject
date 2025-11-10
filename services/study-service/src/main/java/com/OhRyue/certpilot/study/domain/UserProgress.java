package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity @Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(
    name = "user_progress",
    indexes = {
        @Index(name = "idx_user_topic", columnList = "user_id,topic_id")
    }
)
public class UserProgress {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Column(name = "topic_id", nullable = false)
  private Long topicId;

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", length = 20)
  private ExamMode examMode;

  @Column(name = "mini_total")
  private int miniTotal;      // 출제 수(미니체크)

  @Column(name = "mini_correct")
  private int miniCorrect;    // 정답 수(미니체크)

  @Column(name = "mini_passed")
  private boolean miniPassed; // 모두 정답(통과)

  @Column(name = "mcq_total")
  private int mcqTotal;       // 객관식 출제 수

  @Column(name = "mcq_correct")
  private int mcqCorrect;     // 객관식 정답 수

  @Column(name = "updated_at")
  private Instant updatedAt;
}
