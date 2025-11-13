package com.OhRyue.certpilot.cert.domain;

import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "cert_subject")
@Getter
@Setter
public class CertSubjectEntity {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "cert_id", nullable = false)
  private Long certId;

  @Enumerated(EnumType.STRING)
  @Column(name = "exam_mode", nullable = false, length = 16)
  private ExamMode examMode;

  @Column(nullable = false, length = 255)
  private String name;

  @Column(name = "total_questions")
  private Integer totalQuestions;

  @Column(name = "duration_minutes")
  private Integer durationMinutes;

  @Column(name = "subject_seq", nullable = false)
  private Integer subjectSeq;
}

