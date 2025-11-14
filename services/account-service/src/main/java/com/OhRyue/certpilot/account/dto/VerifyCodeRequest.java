package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class VerifyCodeRequest {

    @Email
    @NotBlank(message = "이메일은 필수입니다.")
    private String email; // 코드가 발송된 이메일

    @NotBlank(message = "인증 코드는 필수입니다.")
    private String code;  // 사용자가 입력한 인증 코드
}
