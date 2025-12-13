package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity
@Table(name = "user_skill_counter")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSkillCounter {
  
  @Id
  @Column(name = "user_id", length = 100, nullable = false)
  private String userId;
  
  @Column(name = "first_study_completed", nullable = false)
  private Boolean firstStudyCompleted = false;
  
  @Column(name = "written_review_90_cnt", nullable = false)
  private Integer writtenReview90Cnt = 0;
  
  @Column(name = "practical_micro_100_cnt", nullable = false)
  private Integer practicalMicro100Cnt = 0;
  
  @Column(name = "accuracy_80_cnt", nullable = false)
  private Integer accuracy80Cnt = 0;
  
  @Column(name = "duel_streak", nullable = false)
  private Integer duelStreak = 0;
  
  @Column(name = "tournament_wins", nullable = false)
  private Integer tournamentWins = 0;
  
  @Column(name = "goldenbell_wins", nullable = false)
  private Integer goldenbellWins = 0;
  
  @Column(name = "updated_at", nullable = false)
  private Instant updatedAt;
  
  @PrePersist
  @PreUpdate
  void pre() {
    if (updatedAt == null) {
      updatedAt = Instant.now();
    }
  }
}

















