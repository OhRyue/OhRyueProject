package com.OhRyue.certpilot.curriculum.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "topic")
public class Topic {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "cert_id", nullable = false)
  private Long certId;

  @Column(nullable = false, length = 50)
  private String code;

  @Column(nullable = false, length = 200)
  private String name;

  @Column(nullable = false)
  private Integer level;

  @Column(name = "parent_id")
  private Long parentId;

  @Column(nullable = false)
  private Integer ord = 0;

  public boolean isLeaf() {
    return level != null && level == 4;
  }
}

