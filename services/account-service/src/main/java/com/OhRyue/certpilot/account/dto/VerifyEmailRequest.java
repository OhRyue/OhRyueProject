package com.OhRyue.certpilot.account.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class VerifyEmailRequest {
    private String email;
    private String code;
    private String username;
    private String password;
}
