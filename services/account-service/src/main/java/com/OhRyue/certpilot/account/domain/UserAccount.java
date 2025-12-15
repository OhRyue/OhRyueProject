package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * 최신 스키마 매핑: user_account
 * - PK: id (VARCHAR)
 * - email UNIQUE
 * - password_hash
 * - status ENUM('ACTIVE','BLOCKED','DELETED')
 * - signup_type ENUM('EMAIL','SOCIAL')
 * - created_at, last_login_at
 */
@Entity
@Table(name = "user_account")
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserAccount {

  @Id
  @Column(length = 100, nullable = false)
  private String id; // 예: 'ohryue' (username을 PK로 사용)

  @Column(nullable = true, unique = true, length = 255)
  private String email;

  @Column(name = "password_hash", nullable = true, length = 255)
  private String passwordHash;

  @Builder.Default
  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private AccountStatus status = AccountStatus.BLOCKED;

  @Builder.Default
  @Enumerated(EnumType.STRING)
  @Column(name = "signup_type", nullable = false, length = 16)
  private SignupType signupType = SignupType.EMAIL;

  @Column(name = "created_at", nullable = false)
  private LocalDateTime createdAt;

  @Column(name = "last_login_at")
  private LocalDateTime lastLoginAt;

  public boolean isActive() {
    return status == AccountStatus.ACTIVE;
  }
}
