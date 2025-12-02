package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class WithdrawRequest {

    @NotBlank(message = "비밀번호는 필수입니다.")
    private String password;
}

