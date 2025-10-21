package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "micro_result_item")
@Getter
@NoArgsConstructor
public class MicroResultItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long resultId;

  @Column(length = 8, nullable = false)
  private String itemType;   // MINI | QUIZ

  private Long refId;

  private boolean correct;

  private Integer chosenIdx;

  private Integer answerIdx;

  @Column(length = 64)
  private String tag;

  @Lob
  private String expSnapshot;

  @Column(nullable = false, updatable = false, insertable = false, columnDefinition = "timestamp default current_timestamp")
  private Instant createdAt;

  public MicroResultItem(Long resultId, String itemType, Long refId, boolean correct,
                         Integer chosenIdx, Integer answerIdx, String tag, String expSnapshot) {
    this.resultId = resultId;
    this.itemType = itemType;
    this.refId = refId;
    this.correct = correct;
    this.chosenIdx = chosenIdx;
    this.answerIdx = answerIdx;
    this.tag = tag;
    this.expSnapshot = expSnapshot;
  }
}
