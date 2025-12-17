package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class KakaoLoginRequest {
    @NotBlank(message = "카카오 access_token이 필요합니다.")
    private String accessToken;
}
