package com.OhRyue.certpilot.account.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class VerifyEmailRequest {
  private String email;
  private String code; // 사용자가 입력한 인증코드
}
