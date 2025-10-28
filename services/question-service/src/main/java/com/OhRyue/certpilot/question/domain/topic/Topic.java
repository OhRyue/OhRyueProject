package com.OhRyue.certpilot.question.domain.topic;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "topic", indexes = {
    @Index(name = "idx_topic_parent", columnList = "parent_id")
})
public class Topic {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable=false, length=120)
  private String name;

  @Column(nullable=false)
  private Integer level; // 1~4

  @Column(name="parent_id")
  private Long parentId;

  @Column(nullable=false)
  private Integer ord = 0;
}
