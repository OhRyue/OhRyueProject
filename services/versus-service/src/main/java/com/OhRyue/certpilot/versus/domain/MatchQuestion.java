package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "match_question")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MatchQuestion {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "room_id", nullable = false)
  private Long roomId;

  @Column(name = "round_no", nullable = false)
  private Integer roundNo;

  @Enumerated(EnumType.STRING)
  @Column(name = "phase", nullable = false, length = 16)
  private MatchPhase phase;

  @Column(name = "order_no", nullable = false)
  private Integer orderNo;

  @Column(name = "question_id", nullable = false)
  private Long questionId;

  @Column(name = "time_limit_sec", nullable = false)
  private Integer timeLimitSec;
}
 