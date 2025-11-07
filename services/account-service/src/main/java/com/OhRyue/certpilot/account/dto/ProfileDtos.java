package com.OhRyue.certpilot.account.dto;

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
    private String nickname;
    private String avatarUrl;
    private String timezone;
    private String lang;
  }
}
