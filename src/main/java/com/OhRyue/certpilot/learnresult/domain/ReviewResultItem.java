package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "review_result_item")
@Getter
@NoArgsConstructor
public class ReviewResultItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long resultId;

  private Long questionId;

  private boolean correct;

  private Integer chosenIdx;

  private Integer answerIdx;

  @Column(length = 64)
  private String tag;

  @Lob
  private String expSnapshot;

  @Column(nullable = false, updatable = false, insertable = false, columnDefinition = "timestamp default current_timestamp")
  private Instant createdAt;

  public ReviewResultItem(Long resultId, Long questionId, boolean correct,
                          Integer chosenIdx, Integer answerIdx, String tag, String expSnapshot) {
    this.resultId = resultId;
    this.questionId = questionId;
    this.correct = correct;
    this.chosenIdx = chosenIdx;
    this.answerIdx = answerIdx;
    this.tag = tag;
    this.expSnapshot = expSnapshot;
  }
}
