package com.OhRyue.certpilot.account.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
@Schema(description = "회원가입/로그인 응답")
public class UserResponseDto {

    @Schema(example = "로그인 성공")
    private String message;

    @Schema(example = "1")
    private Long userId;

    @Schema(example = "ohryue")
    private String username;

    @Schema(example = "USER")
    private String role;
}
