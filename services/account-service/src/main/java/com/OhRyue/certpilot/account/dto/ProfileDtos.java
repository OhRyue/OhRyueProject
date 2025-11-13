package com.OhRyue.certpilot.account.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.*;

public class ProfileDtos {

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class ProfileResponse {
    private String userId;
    private String nickname;
    private String avatarUrl;
    private String timezone;
    private String lang;
  }

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class ProfileUpdateRequest {
    @NotBlank
    private String nickname;
    private String avatarUrl;
    private String timezone;
    private String lang;
  }
}
