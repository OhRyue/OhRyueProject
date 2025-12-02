package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "user_goal_cert",
    indexes = {
        @Index(name="ix_goal_cert", columnList = "cert_id"),
        @Index(name="ix_goal_round", columnList = "target_round_id")
    },
    uniqueConstraints = {
        @UniqueConstraint(name="uq_goal_user", columnNames = "user_id")
    })
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserGoalCert {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(name="cert_id", nullable=false)
  private Long certId;

  @Enumerated(EnumType.STRING)
  @Column(name="target_exam_mode", nullable=false, length=16)
  private ExamMode targetExamMode;

  @Column(name="target_round_id")
  private Long targetRoundId;

  @Column(name="target_exam_date")
  private LocalDate targetExamDate;

  @Column(name="dday_cached")
  private Integer ddayCached;

  @Column(name="created_at", nullable=false)
  private LocalDateTime createdAt;
}
