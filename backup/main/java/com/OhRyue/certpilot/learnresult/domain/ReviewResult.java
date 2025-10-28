package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
@Entity
@Table(name = "review_result")
public class ReviewResult {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long userId;
  private Long certId;
  private Long detailTopicId;
  private int score;
  private int total;

  @Lob
  private String aiSummary;

  @Column(name = "created_at", updatable = false, insertable = false)
  private Instant createdAt;
}
