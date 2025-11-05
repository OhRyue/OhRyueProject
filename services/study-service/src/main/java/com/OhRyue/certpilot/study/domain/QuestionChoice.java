package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "question_choice",
    indexes = {
        @Index(name = "ix_qchoice_qid", columnList = "questionId"),
        @Index(name = "ix_qchoice_label", columnList = "label")
    })
public class QuestionChoice {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false)
  private Long questionId;

  @Column(nullable = false, length = 8)
  private String label;       // A/B/C/D ...

  @Column(columnDefinition = "TEXT")
  private String text;

  @Column(nullable = false)
  private Boolean correct;
}
