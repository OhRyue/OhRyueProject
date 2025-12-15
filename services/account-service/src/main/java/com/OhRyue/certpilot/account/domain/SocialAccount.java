package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

/**
 * 소셜 로그인 계정 정보
 * - user_account와 1:N 관계
 * - provider와 provider_id의 조합은 유일해야 함
 */
@Entity
@Table(name = "social_account")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SocialAccount {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false, length = 100)
    private String userId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private SocialProvider provider;

    @Column(name = "provider_id", nullable = false, length = 255)
    private String providerId;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;
}
