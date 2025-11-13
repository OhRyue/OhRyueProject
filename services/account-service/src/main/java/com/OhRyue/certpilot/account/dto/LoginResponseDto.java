package com.OhRyue.certpilot.account.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class LoginResponseDto {
  private String accessToken;
  private String refreshToken;
  private String userId;
  private String email;
  private String role;
}
