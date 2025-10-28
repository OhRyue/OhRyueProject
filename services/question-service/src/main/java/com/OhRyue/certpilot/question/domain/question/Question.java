package com.OhRyue.certpilot.question.domain.question;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "question", indexes = {
    @Index(name="idx_question_level", columnList = "difficulty")
})
public class Question {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable=false, length=400)
  private String stem;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name="choices_json", columnDefinition="JSON", nullable=false)
  private String choicesJson; // ["A","B","C","D"] 문자열 JSON

  @Column(name="answer_idx", nullable=false)
  private Integer answerIdx; // 0-based

  @Column(columnDefinition="TEXT")
  private String exp; // 해설

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name="meta_json", columnDefinition="JSON")
  private String metaJson;

  private Integer difficulty; // 1~5 등급
}
