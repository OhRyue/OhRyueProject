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

  @Column(name = "avatar_url", length = 500)
  private String avatarUrl;

  @Column(length = 64)
  private String timezone;

  @Column(length = 16)
  private String lang;
}
