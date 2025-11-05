package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "question_tag", indexes = {
    @Index(name = "ix_qtag_tag", columnList = "tag"),
    @Index(name = "ix_qtag_qid", columnList = "questionId")
})
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuestionTag {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false)
  private Long questionId;

  @Column(nullable = false, length = 80)
  private String tag;
}
