package com.OhRyue.certpilot.learnresult.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
@Entity
@Table(name = "micro_result_item")
public class MicroResultItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  private Long resultId;
  private String itemType;   // "MINI" | "QUIZ"
  private Long refId;        // concept_check.id or question.id
  private Integer chosenIdx; // null 허용
  private boolean correct;

  @Lob
  private String explanation; // MINI: DB 해설, QUIZ: AI→폴백

  private int ordNo;

  @Column(name = "created_at", updatable = false, insertable = false)
  private Instant createdAt;
}
