package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "topic",
    indexes = {
        @Index(name = "ix_topic_parent", columnList = "parentId"),
        @Index(name = "ix_topic_exam_mode", columnList = "examMode")
    })
public class Topic {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** 계층(major/sub/micro) */
  @Column(name = "parentId")
  private Long parentId;

  /** 예: "1.1.1" */
  @Column(name = "code", length = 50)
  private String code;

  /** 예: "현행 시스템 분석" */
  @Column(name = "title", length = 200, nullable = false)
  private String title;

  /** WRITTEN | PRACTICAL */
  @Enumerated(EnumType.STRING)
  @Column(name = "examMode", length = 20, nullable = false)
  private ExamMode examMode;
}
