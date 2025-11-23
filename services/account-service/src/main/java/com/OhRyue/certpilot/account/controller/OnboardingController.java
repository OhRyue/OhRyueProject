package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingProfileRequest;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.service.OnboardingService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
  public ResponseEntity<OnboardingStatusResponse> status() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    return ResponseEntity.ok(onboardingService.getStatus(userId));
  }

  @Operation(summary = "온보딩 프로필/목표 자격증 설정 (닉네임 + 자격증 선택)")
  @PostMapping("/profile")
  public ResponseEntity<OnboardingStatusResponse> setupProfile(
      @Valid @RequestBody OnboardingProfileRequest request
  ) {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    OnboardingStatusResponse status = onboardingService.updateProfileAndGoal(userId, request);
    return ResponseEntity.ok(status);
  }

  @Operation(summary = "온보딩 완료 처리")
  @PostMapping("/complete")
  public ResponseEntity<OnboardingStatusResponse> complete() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    return ResponseEntity.ok(onboardingService.markComplete(userId));
  }
}
