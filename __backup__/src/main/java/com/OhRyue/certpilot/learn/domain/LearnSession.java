package com.OhRyue.certpilot.learn.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "learn_session")
public class LearnSession {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", nullable=false)
  private Long userId;

  @Column(name="concept_id", nullable=false)
  private Long conceptId;

  @Column(name="status", length=20)
  private String status = "OPEN";

  @Column(name="created_at", insertable = false, updatable = false)
  private Instant createdAt;

  public LearnSession(Long userId, Long conceptId) {
    this.userId = userId;
    this.conceptId = conceptId;
  }

  public void close() { this.status = "DONE"; }
}
