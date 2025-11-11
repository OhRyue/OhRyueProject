package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.Pattern;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserRegisterDto {
    @Pattern(regexp = "^[A-Za-z0-9]{8,20}$", message = "아이디는 영문과 숫자 8~20자여야 합니다.")
    private String username;
    private String password;
    private String email;
}