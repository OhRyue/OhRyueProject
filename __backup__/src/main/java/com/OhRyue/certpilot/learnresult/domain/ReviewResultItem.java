package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
@Entity
@Table(name = "review_result_item")
public class ReviewResultItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long resultId;
  private Long questionId;
  private Integer chosenIdx;
  private boolean correct;

  @Lob
  private String aiExplanation;

  private int ordNo;

  @Column(name = "created_at", updatable = false, insertable = false)
  private Instant createdAt;
}
