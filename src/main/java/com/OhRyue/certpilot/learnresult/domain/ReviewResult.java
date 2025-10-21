package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "review_result")
@Getter
@NoArgsConstructor
public class ReviewResult {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long userId;

  private Long detailTopicId;

  private int score;

  private int total;

  @Column(nullable = false, updatable = false, insertable = false, columnDefinition = "timestamp default current_timestamp")
  private Instant createdAt;

  public ReviewResult(Long userId, Long detailTopicId, int score, int total) {
    this.userId = userId;
    this.detailTopicId = detailTopicId;
    this.score = score;
    this.total = total;
  }
}
