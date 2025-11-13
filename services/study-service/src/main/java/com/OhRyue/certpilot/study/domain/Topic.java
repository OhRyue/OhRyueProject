package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "topic",
    indexes = {
        @Index(name = "ix_topic_parent", columnList = "parent_id"),
        @Index(name = "ix_topic_mode", columnList = "exam_mode")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Topic {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "cert_id", nullable = false)
  private Long certId;

  @Column(name = "parent_id")
  private Long parentId;

  @Column(name = "code", length = 50, nullable = false)
  private String code;

  @Column(name = "title", length = 200, nullable = false)
  private String title;

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", length = 20, nullable = false)
  private ExamMode examMode;

  @Column(name = "emoji", length = 10)
  private String emoji;

  @Column(name = "order_no")
  private Integer orderNo;

  @Column(name = "created_at", nullable = false)
  private Instant createdAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    if (createdAt == null) createdAt = now;
    if (updatedAt == null) updatedAt = now;
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}
