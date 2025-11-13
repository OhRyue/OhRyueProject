package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;

@Entity @Table(name="report_tag_skill", indexes = {
    @Index(name="ix_rtag_user", columnList="user_id"),
    @Index(name="ix_rtag_tag", columnList="tag")
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class ReportTagSkill {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(length=100, nullable=false)
  private String tag;

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", length=10, nullable=false)
  private ExamMode examMode;

  @Column(nullable=false)
  private int correct;

  @Column(nullable=false)
  private int total;

  @Column(nullable=false, precision=5, scale=2)
  private BigDecimal accuracy;

  @Column(nullable=false)
  private Instant updatedAt;

  @PrePersist @PreUpdate
  void touch(){ updatedAt = Instant.now(); }
}
