package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "user_profile")
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserProfile {

  @Id
  @Column(name = "user_id", length = 100)
  private String userId;

  @Column(nullable = false, length = 100)
  private String nickname;

  @Column(name = "skin_id")
  private Long skinId;

  @Column(length = 64)
  private String timezone;

  @Column(length = 16)
  private String lang;

  @Column(name = "onboarding_completed", nullable = false)
  @Builder.Default
  private Boolean onboardingCompleted = false;
}
