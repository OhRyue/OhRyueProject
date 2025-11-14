package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingProfileRequest;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.service.OnboardingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/account/onboarding")
@Tag(name = "Account - Onboarding", description = "온보딩 단계 상태 조회 및 완료 처리")
@RequiredArgsConstructor
public class OnboardingController {

    private final OnboardingService onboardingService;

    @Operation(summary = "온보딩 진행 상태 조회")
    @GetMapping("/status")
    public ResponseEntity<OnboardingStatusResponse> status(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(401).build();
        }
        String userId = authentication.getName();
        return ResponseEntity.ok(onboardingService.getStatus(userId));
    }

    @Operation(summary = "온보딩 프로필/목표 자격증 설정 (닉네임 + 자격증 선택)")
    @PostMapping("/profile")
    public ResponseEntity<OnboardingStatusResponse> setupProfile(
            Authentication authentication,
            @Valid @RequestBody OnboardingProfileRequest request
    ) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(401).build();
        }
        String userId = authentication.getName();
        OnboardingStatusResponse status = onboardingService.updateProfileAndGoal(userId, request);
        return ResponseEntity.ok(status);
    }

    @Operation(summary = "온보딩 완료 처리")
    @PostMapping("/complete")
    public ResponseEntity<OnboardingStatusResponse> complete(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(401).build();
        }
        String userId = authentication.getName();
        return ResponseEntity.ok(onboardingService.markComplete(userId));
    }
}
