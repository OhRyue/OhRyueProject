package com.OhRyue.certpilot.account.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class VerifyCodeRequest {
    private String email; // 인증받은 이메일
    private String code;  // 사용자가 입력한 인증 코드
}
