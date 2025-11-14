package com.OhRyue.certpilot.account.dto;

import com.OhRyue.certpilot.account.domain.ExamMode;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

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

    /**
     * 온보딩 단계에서 프로필(닉네임 등)과 목표 자격증을 한 번에 설정하기 위한 요청 DTO
     */
    @Getter
    @Setter
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public static class OnboardingProfileRequest {

        @Schema(description = "설정할 닉네임", example = "코딩하는펭귄")
        @NotBlank(message = "닉네임은 필수입니다.")
        private String nickname;

        @Schema(description = "아바타 이미지 URL", example = "https://example.com/avatar.png")
        private String avatarUrl;

        @Schema(description = "시간대", example = "Asia/Seoul")
        private String timezone;

        @Schema(description = "언어 코드", example = "ko-KR")
        private String lang;

        @Schema(description = "목표 자격증 ID", example = "1")
        @NotNull(message = "목표 자격증 ID는 필수입니다.")
        private Long certId;

        @Schema(description = "목표 시험 모드(WRITTEN/PRACTICAL)", example = "WRITTEN")
        @NotNull(message = "목표 시험 모드는 필수입니다.")
        private ExamMode targetExamMode;

        @Schema(description = "목표 회차 ID (선택)")
        private Long targetRoundId;
    }
}
