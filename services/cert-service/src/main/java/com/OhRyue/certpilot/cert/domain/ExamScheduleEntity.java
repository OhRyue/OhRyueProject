package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;
import java.time.LocalDate;

@Entity
@Table(name = "exam_schedule")
@Getter
@Setter
public class ExamScheduleEntity {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "source", nullable = false, length = 20)
  private String source;

  @Column(name = "impl_yy", length = 4)
  private String implYy;

  @Column(name = "impl_seq", length = 10)
  private String implSeq;

  @Column(name = "qualgb_cd", length = 10)
  private String qualgbCd;

  @Column(name = "qualgb_nm")
  private String qualgbNm;

  @Column(name = "jm_cd", length = 20)
  private String jmCd;

  @Column(name = "jm_nm")
  private String jmNm;

  @Column(name = "description", columnDefinition = "TEXT")
  private String description;

  @Column(name = "doc_reg_start_dt")
  private LocalDate docRegStartDt;

  @Column(name = "doc_reg_end_dt")
  private LocalDate docRegEndDt;

  @Column(name = "doc_exam_start_dt")
  private LocalDate docExamStartDt;

  @Column(name = "doc_exam_end_dt")
  private LocalDate docExamEndDt;

  @Column(name = "doc_pass_dt")
  private LocalDate docPassDt;

  @Column(name = "prac_reg_start_dt")
  private LocalDate pracRegStartDt;

  @Column(name = "prac_reg_end_dt")
  private LocalDate pracRegEndDt;

  @Column(name = "prac_exam_start_dt")
  private LocalDate pracExamStartDt;

  @Column(name = "prac_exam_end_dt")
  private LocalDate pracExamEndDt;

  @Column(name = "prac_pass_dt")
  private LocalDate pracPassDt;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    Instant now = Instant.now();
    this.createdAt = now;
    this.updatedAt = now;
  }

  @PreUpdate
  void onUpdate() {
    this.updatedAt = Instant.now();
  }
}

