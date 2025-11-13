package com.OhRyue.certpilot.account.dto;

import com.OhRyue.certpilot.account.dto.GoalCertDtos.GoalResponse;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.SettingsDtos.SettingsResponse;
import lombok.Builder;
import lombok.Getter;

public class AccountOverviewDtos {

  @Getter
  @Builder
  public static class AccountSummary {
    private String userId;
    private String email;
    private String status;
  }

  @Getter
  @Builder
  public static class MeResponse {
    private AccountSummary account;
    private ProfileResponse profile;
    private SettingsResponse settings;
    private GoalResponse goal;
    private OnboardingStatusResponse onboarding;
  }
}


