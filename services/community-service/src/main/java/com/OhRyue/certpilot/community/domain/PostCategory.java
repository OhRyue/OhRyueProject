package com.OhRyue.certpilot.community.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "post_category")
@Getter
@Setter
public class PostCategory {

  @Id
  private Byte id;

  @Column(nullable = false, unique = true, length = 32)
  private String code;

  @Column(nullable = false, length = 50)
  private String name;

  /**
   * 화면 표시 순서
   * - V3 마이그레이션에서 ADD COLUMN sort_order TINYINT NOT NULL DEFAULT 0;
   * - FREE, QNA, TIP, STUDY, REVIEW 순으로 값 설정
   */
  @Column(name = "sort_order", nullable = false)
  private Byte sortOrder;
}
