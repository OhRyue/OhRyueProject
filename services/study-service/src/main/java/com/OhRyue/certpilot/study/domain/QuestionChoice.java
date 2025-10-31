package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity @Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "question_choice")
public class QuestionChoice {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long questionId;
  private String label;       // A/B/C/D
  @Column(columnDefinition = "TEXT")
  private String text;
  private boolean correct;
}
