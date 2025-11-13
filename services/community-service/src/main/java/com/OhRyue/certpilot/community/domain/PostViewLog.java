package com.OhRyue.certpilot.community.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "post_view_log")
@Getter
@Setter
public class PostViewLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "post_id", nullable = false)
  private Long postId;

  @Column(name = "user_id", length = 100)
  private String userId;

  @Column(name = "viewed_at", nullable = false, updatable = false)
  private Instant viewedAt;

  @PrePersist
  void onCreate() {
    if (viewedAt == null) {
      viewedAt = Instant.now();
    }
  }
}


