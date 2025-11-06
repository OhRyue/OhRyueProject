package com.OhRyue.certpilot.account.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class LoginResponseDto {
    private String message;  // "로그인 성공"
    private String token;    // JWT 토큰
    private String refreshToken;
    private Long userId;
    private String username;
    private String role;
}
