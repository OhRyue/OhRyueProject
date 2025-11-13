package com.OhRyue.certpilot.account.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.time.Instant;

public final class OnboardingDtos {

  private OnboardingDtos() {}

  public enum OnboardingStep {
    VERIFY_EMAIL,
    SET_NICKNAME,
    SELECT_CERT,
    CONFIGURE_SETTINGS
  }

  @Getter
  @Builder
  @JsonInclude(JsonInclude.Include.NON_NULL)
  public static class OnboardingStatusResponse {

    @Schema(description = "이메일 인증 여부")
    private final boolean emailVerified;

    @Schema(description = "닉네임 설정 여부")
    private final boolean nicknameSet;

    @Schema(description = "목표 자격증 선택 여부")
    private final boolean goalSelected;

    @Schema(description = "환경설정 준비 여부")
    private final boolean settingsReady;

    @Schema(description = "온보딩 전체 완료 여부")
    private final boolean completed;

    @Schema(description = "온보딩 완료 시각 (UTC)")
    private final Instant completedAt;

    @Schema(description = "다음 진행해야 할 온보딩 단계")
    private final OnboardingStep nextStep;

    public boolean isCompleted() {
      return completed;
    }
  }
}

