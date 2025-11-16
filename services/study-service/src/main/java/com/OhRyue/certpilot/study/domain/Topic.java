package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.Getter;

@Entity
@Table(name = "topic")
@Getter
public class Topic {

  @Id
  private Long id;

  @Column(name = "cert_id")
  private Long certId;

  @Column(name = "parent_id")
  private Long parentId;

  private String code;

  private String title;

  private String emoji;

  @Column(name = "exam_mode")
  private String examMode; // 'WRITTEN', 'PRACTICAL'

  @Column(name = "order_no")
  private Integer orderNo;
}
