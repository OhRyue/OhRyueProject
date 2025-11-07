package com.OhRyue.certpilot.account.dto;

import lombok.*;

public class SettingsDtos {

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class SettingsResponse {
    private String userId;
    private String uiPrefsJson;
    private String notifPrefsJson;
  }

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class SettingsUpdateRequest {
    private String uiPrefsJson;      // 프론트에서 JSON 문자열로 전달
    private String notifPrefsJson;
  }
}
