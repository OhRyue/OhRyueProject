package com.OhRyue.certpilot.progress.feign.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public record AccountMeResponse(
    AccountSummary account,
    Profile profile,
    Settings settings,
    Goal goal,
    Onboarding onboarding
) {

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record AccountSummary(String userId, String email, String status) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Profile(String userId, String nickname, Long skinId, String timezone, String lang) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Settings(String userId, String uiPrefsJson, String notifPrefsJson) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Goal(Long id, String userId, Long certId, String targetExamMode, Long targetRoundId, Integer ddayCached) {}

  @JsonIgnoreProperties(ignoreUnknown = true)
  public record Onboarding(
      boolean emailVerified,
      boolean nicknameSet,
      boolean goalSelected,
      boolean settingsReady,
      boolean completed,
      String completedAt,
      String nextStep
  ) {}
}

