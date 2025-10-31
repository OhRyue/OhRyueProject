package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity @Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "concept")
public class Concept {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long topicId;         // micro 토픽 기준
  @Column(columnDefinition = "TEXT")
  private String content;       // 개념(마크다운/HTML)
}
