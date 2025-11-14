package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.domain.UserOnboarding;
import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingProfileRequest;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStep;
import com.OhRyue.certpilot.account.repo.UserOnboardingRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.time.Instant;
import java.util.EnumSet;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class OnboardingService {

    private final UserService userService;
    private final ProfileService profileService;
    private final SettingsService settingsService;
    private final GoalCertService goalCertService;
    private final UserOnboardingRepository onboardingRepository;

    public OnboardingStatusResponse getStatus(String userId) {
        UserAccount account = userService.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("사용자를 찾을 수 없습니다."));
        UserProfile profile = profileService.get(userId);
        var settings = settingsService.getSnapshot(userId);
        Optional<UserGoalCert> goalOptional = goalCertService.getByUser(userId);

        boolean emailVerified = account.isActive();
        boolean nicknameSet = hasNickname(profile);
        boolean settingsReady = settings != null;
        boolean goalSelected = goalOptional.isPresent();

        EnumSet<OnboardingStep> incomplete = EnumSet.noneOf(OnboardingStep.class);
        if (!emailVerified) {
            incomplete.add(OnboardingStep.VERIFY_EMAIL);
        }
        if (!nicknameSet) {
            incomplete.add(OnboardingStep.SET_NICKNAME);
        }
        if (!goalSelected) {
            incomplete.add(OnboardingStep.SELECT_CERT);
        }
        if (!settingsReady) {
            incomplete.add(OnboardingStep.CONFIGURE_SETTINGS);
        }

        boolean completed = emailVerified && nicknameSet && goalSelected && settingsReady;

        UserOnboarding onboarding = onboardingRepository.findById(userId)
                .orElseGet(() -> {
                    UserOnboarding created = new UserOnboarding();
                    created.setUserId(userId);
                    return onboardingRepository.save(created);
                });

        Instant completedAt = onboarding.getCompletedAt();
        if (completed && completedAt == null) {
            completedAt = Instant.now();
            onboarding.setCompletedAt(completedAt);
            onboardingRepository.save(onboarding);
        }

        OnboardingStep nextStep = incomplete.stream().findFirst().orElse(null);

        return OnboardingStatusResponse.builder()
                .emailVerified(emailVerified)
                .nicknameSet(nicknameSet)
                .goalSelected(goalSelected)
                .settingsReady(settingsReady)
                .completed(completed)
                .completedAt(completedAt)
                .nextStep(nextStep)
                .build();
    }

    @Transactional
    public OnboardingStatusResponse markComplete(String userId) {
        OnboardingStatusResponse status = getStatus(userId);
        if (!status.isCompleted()) {
            throw new IllegalStateException("온보딩이 아직 완료되지 않았습니다.");
        }

        UserOnboarding onboarding = onboardingRepository.findById(userId)
                .orElseThrow(() -> new IllegalStateException("온보딩 정보를 찾을 수 없습니다."));

        if (onboarding.getCompletedAt() == null) {
            onboarding.setCompletedAt(Instant.now());
            onboardingRepository.save(onboarding);
        }

        return status;
    }

    /**
     * 온보딩 단계에서 프로필(닉네임 등)과 목표 자격증을 한 번에 설정.
     * - user_profile.nickname / avatar / timezone / lang 업데이트
     * - user_goal_cert upsert
     * - 최종적으로 최신 온보딩 상태 반환
     */
    @Transactional
    public OnboardingStatusResponse updateProfileAndGoal(String userId, OnboardingProfileRequest req) {
        // 1) 프로필 업데이트
        UserProfile profile = profileService.get(userId); // 없으면 기본값 생성
        profile.setNickname(req.getNickname());
        if (StringUtils.hasText(req.getAvatarUrl())) {
            profile.setAvatarUrl(req.getAvatarUrl());
        }
        if (StringUtils.hasText(req.getTimezone())) {
            profile.setTimezone(req.getTimezone());
        }
        if (StringUtils.hasText(req.getLang())) {
            profile.setLang(req.getLang());
        }
        profileService.upsert(profile);

        // 2) 목표 자격증 upsert
        UserGoalCert goal = UserGoalCert.builder()
                .userId(userId)
                .certId(req.getCertId())
                .targetExamMode(req.getTargetExamMode())
                .targetRoundId(req.getTargetRoundId())
                .build();
        goalCertService.upsert(goal);

        // 3) 최신 온보딩 상태 반환
        return getStatus(userId);
    }

    private boolean hasNickname(UserProfile profile) {
        return profile != null && StringUtils.hasText(profile.getNickname());
    }
}
