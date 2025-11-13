package com.OhRyue.certpilot.account.dto;

import jakarta.validation.Valid;
import lombok.*;

public class SettingsDtos {

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class SettingsResponse {
    private String userId;
    private UiPreferences ui;
    private NotificationPreferences notification;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class SettingsUpdateRequest {
    @Valid
    private UiPreferencesUpdate ui;
    @Valid
    private NotificationPreferencesUpdate notification;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class UiPreferences {
    private boolean darkMode;
    private boolean soundEffects;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class NotificationPreferences {
    private boolean pushEnabled;
    private boolean emailEnabled;
    private boolean goalReminderEnabled;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class UiPreferencesUpdate {
    private Boolean darkMode;
    private Boolean soundEffects;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class NotificationPreferencesUpdate {
    private Boolean pushEnabled;
    private Boolean emailEnabled;
    private Boolean goalReminderEnabled;
  }
}
