package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.AccountOverviewDtos.AccountSummary;
import com.OhRyue.certpilot.account.dto.AccountOverviewDtos.MeResponse;
import com.OhRyue.certpilot.account.dto.GoalCertDtos.GoalResponse;
import com.OhRyue.certpilot.account.dto.LoginResponseDto;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.SettingsDtos.SettingsResponse;
import com.OhRyue.certpilot.account.dto.TokenRefreshRequest;
import com.OhRyue.certpilot.account.dto.UserLoginDto;
import com.OhRyue.certpilot.account.dto.UserRegisterDto;
import com.OhRyue.certpilot.account.dto.UserResponseDto;
import com.OhRyue.certpilot.account.dto.VerifyEmailRequest;
import com.OhRyue.certpilot.account.service.EmailService;
import com.OhRyue.certpilot.account.service.GoalCertService;
import com.OhRyue.certpilot.account.service.OnboardingService;
import com.OhRyue.certpilot.account.service.ProfileService;
import com.OhRyue.certpilot.account.service.RefreshTokenService;
import com.OhRyue.certpilot.account.service.SettingsService;
import com.OhRyue.certpilot.account.service.UserService;
import com.OhRyue.certpilot.account.service.VerificationCodeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@Tag(name = "Account - Auth", description = "회원 인증 및 계정 조회 APIs")
@RestController
@RequestMapping("/api/account")
@RequiredArgsConstructor
public class AuthController {

  private final UserService userService;
  private final EmailService emailService;
  private final RefreshTokenService refreshTokenService;
  private final JwtTokenProvider jwtTokenProvider;
  private final VerificationCodeService verificationCodeService;
  private final ProfileService profileService;
  private final SettingsService settingsService;
  private final GoalCertService goalCertService;
  private final OnboardingService onboardingService;

  /* -------- 회원 가입 & 인증 -------- */
  @Operation(summary = "회원 가입 후 이메일 인증코드 발송")
  @PostMapping("/signup")
  @ResponseStatus(HttpStatus.CREATED)
  public UserResponseDto signup(@Valid @RequestBody UserRegisterDto req) {
    UserAccount user = userService.register(req.getEmail(), req.getPassword());

    profileService.get(user.getId());
    settingsService.getSnapshot(user.getId());
    onboardingService.getStatus(user.getId());

    String verificationCode = String.format("%06d", (int) (Math.random() * 1_000_000));
    verificationCodeService.saveCode(user.getEmail(), verificationCode);
    emailService.sendVerificationCode(user.getEmail(), verificationCode);

    return new UserResponseDto(user.getId(), user.getEmail(), user.getStatus().name());
  }

  @Operation(summary = "이메일 인증 코드 검증")
  @PostMapping("/verify-email")
  public ResponseEntity<UserResponseDto> verifyEmail(@Valid @RequestBody VerifyEmailRequest request) {
    String email = normalizeEmail(request.getEmail());
    String savedCode = verificationCodeService.getCode(email);
    if (savedCode == null || !savedCode.equals(request.getCode())) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(new UserResponseDto(null, email, "INVALID_CODE"));
    }

    userService.enableUser(email);
    verificationCodeService.deleteCode(email);
    UserAccount account = userService.findById(email)
        .orElseThrow(() -> new IllegalStateException("사용자를 찾을 수 없습니다."));

    return ResponseEntity.ok(new UserResponseDto(account.getId(), account.getEmail(), account.getStatus().name()));
  }

  /* -------- 로그인 & 토큰 관리 -------- */
  @Operation(summary = "로그인 및 토큰 발급")
  @PostMapping("/login")
  public ResponseEntity<LoginResponseDto> login(@Valid @RequestBody UserLoginDto req) {
    UserAccount user = userService.login(req.getEmail(), req.getPassword());

    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    return ResponseEntity.ok(new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER"
    ));
  }

  @Operation(summary = "Access Token 재발급")
  @PostMapping("/refresh")
  public ResponseEntity<Map<String, String>> refresh(@Valid @RequestBody TokenRefreshRequest request) {
    String refreshToken = request.getRefreshToken();
    if (!jwtTokenProvider.validateToken(refreshToken)) {
      throw new IllegalArgumentException("리프레시 토큰이 유효하지 않습니다");
    }
    String username = jwtTokenProvider.getUsernameFromToken(refreshToken);
    String savedToken = refreshTokenService.get(username);
    if (!refreshToken.equals(savedToken)) {
      throw new IllegalArgumentException("리프레시 토큰이 일치하지 않습니다 (재로그인 필요)");
    }
    String newAccessToken = jwtTokenProvider.generateToken(username);
    return ResponseEntity.ok(Map.of("accessToken", newAccessToken));
  }

  @Operation(summary = "로그아웃 및 Refresh Token 제거")
  @PostMapping("/logout")
  public ResponseEntity<Void> logout(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String username = authentication.getName();
    refreshTokenService.delete(username);
    return ResponseEntity.noContent().build();
  }

  /* -------- 내 정보 조회 -------- */
  @Operation(summary = "계정/프로필/설정/목표 통합 조회")
  @GetMapping("/me")
  public ResponseEntity<MeResponse> me(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String userId = authentication.getName();

    UserAccount account = userService.findById(userId)
        .orElseThrow(() -> new IllegalStateException("사용자를 찾을 수 없습니다."));

    UserProfile profile = profileService.get(userId);
    SettingsResponse settings = settingsService.getSnapshot(userId);
    GoalResponse goal = goalCertService.getByUser(userId)
        .map(this::mapGoal)
        .orElse(null);
    OnboardingStatusResponse onboarding = onboardingService.getStatus(userId);

    MeResponse body = MeResponse.builder()
        .account(AccountSummary.builder()
            .userId(account.getId())
            .email(account.getEmail())
            .status(account.getStatus().name())
            .build())
        .profile(ProfileResponse.builder()
            .userId(profile.getUserId())
            .nickname(profile.getNickname())
            .avatarUrl(profile.getAvatarUrl())
            .timezone(profile.getTimezone())
            .lang(profile.getLang())
            .build())
        .settings(settings)
        .goal(goal)
        .onboarding(onboarding)
        .build();

    return ResponseEntity.ok(body);
  }

  private String normalizeEmail(String email) {
    return email == null ? null : email.trim().toLowerCase();
  }

  private GoalResponse mapGoal(UserGoalCert goal) {
    return GoalResponse.builder()
        .id(goal.getId())
        .userId(goal.getUserId())
        .certId(goal.getCertId())
        .targetExamMode(goal.getTargetExamMode())
        .targetRoundId(goal.getTargetRoundId())
        .ddayCached(goal.getDdayCached())
        .build();
  }
}
