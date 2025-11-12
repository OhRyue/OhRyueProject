package com.OhRyue.certpilot.account.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ResetPasswordRequest {
    private String email;       // 이메일
    private String newPassword; // 새 비밀번호
}
