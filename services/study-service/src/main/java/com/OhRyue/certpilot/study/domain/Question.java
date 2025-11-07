package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "question",
    indexes = {
        @Index(name = "ix_q_topic", columnList = "topicId"),
        @Index(name = "ix_q_type", columnList = "type"),
        @Index(name = "ix_q_diff", columnList = "difficulty")
    })
public class Question {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false)
  private Long topicId;     // 출제 토픽(세세항목/세부항목 등)

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private QuestionType type;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private Difficulty difficulty;

  @Column(columnDefinition = "TEXT")
  private String text;

  private String imageUrl;  // 실기용/객관식 이미지(Optional)

  // OX 전용
  private Boolean oxAnswer; // type=OX일 때 정답(O=true, X=false)

  @Column(columnDefinition = "TEXT")
  private String explanation; // 해설
}
