package com.OhRyue.certpilot.account.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class UserResponseDto {
  private String userId;
  private String email;
  private String status;
}
