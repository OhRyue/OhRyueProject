package com.OhRyue.certpilot.account.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class LoginResponseDto {
  private String message;     // "로그인 성공"
  private String token;       // Access JWT
  private String refreshToken;
  private String userId;      // 문자열 PK
  private String username;    // = userId와 동일하게 사용
  private String role;        // "USER"
}
