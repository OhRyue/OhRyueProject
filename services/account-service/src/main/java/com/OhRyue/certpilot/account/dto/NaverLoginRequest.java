package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class NaverLoginRequest {
    @NotBlank(message = "네이버 authorization code가 필요합니다.")
    private String code;
    
    @NotBlank(message = "네이버 state 값이 필요합니다.")
    private String state;
}
