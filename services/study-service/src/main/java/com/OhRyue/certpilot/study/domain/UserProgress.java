package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity @Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "user_progress",
    indexes = { @Index(name="idx_user_topic", columnList = "userId,topicId") })
public class UserProgress {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private String userId;
  private Long topicId;
  @Enumerated(EnumType.STRING)
  private ExamMode examMode;

  private int miniTotal;      // 출제 수(미니체크)
  private int miniCorrect;    // 정답 수(미니체크)
  private boolean miniPassed; // 모두 정답(통과)

  private int mcqTotal;       // 객관식 출제 수
  private int mcqCorrect;     // 객관식 정답 수

  private Instant updatedAt;
}
