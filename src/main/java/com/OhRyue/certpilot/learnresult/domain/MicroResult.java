package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
@Entity
@Table(name = "micro_result")
public class MicroResult {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long userId;
  private Long certId;
  private Long topicId;   // micro topic(level=4)
  private Long conceptId;
  private int score;
  private int total;

  @Column(name = "created_at", updatable = false, insertable = false)
  private Instant createdAt;
}
