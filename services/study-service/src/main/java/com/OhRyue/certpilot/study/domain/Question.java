package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import com.OhRyue.certpilot.study.domain.enums.QuestionType;
import jakarta.persistence.*;
import lombok.*;

@Entity @Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "question")
public class Question {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long topicId;     // 출제 토픽(세세항목/세부항목 등)
  @Enumerated(EnumType.STRING)
  private QuestionType type;

  @Enumerated(EnumType.STRING)
  private Difficulty difficulty;

  @Column(columnDefinition = "TEXT")
  private String text;

  private String imageUrl;  // 실기용 이미지(Optional)

  // OX 전용
  private Boolean oxAnswer; // type=OX일 때 정답(O=true, X=false)

  @Column(columnDefinition = "TEXT")
  private String explanation; // 해설
}
