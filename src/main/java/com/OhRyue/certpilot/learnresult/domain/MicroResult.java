package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "micro_result")
@Getter
@NoArgsConstructor
public class MicroResult {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long userId;

  private Long conceptId;

  private Long topicId;

  private int score;

  private int total;

  @Column(nullable = false, updatable = false, insertable = false, columnDefinition = "timestamp default current_timestamp")
  private Instant createdAt;

  public MicroResult(Long userId, Long conceptId, Long topicId, int score, int total) {
    this.userId = userId;
    this.conceptId = conceptId;
    this.topicId = topicId;
    this.score = score;
    this.total = total;
  }
}
