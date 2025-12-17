package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class ConnectSocialAccountRequest {
    @NotBlank(message = "구글 id_token이 필요합니다.")
    private String idToken;
}
