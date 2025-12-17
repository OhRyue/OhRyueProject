package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.domain.UserGoalCert;
import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.AccountOverviewDtos.AccountSummary;
import com.OhRyue.certpilot.account.dto.AccountOverviewDtos.MeResponse;
import com.OhRyue.certpilot.account.dto.ForgotPasswordRequest;
import com.OhRyue.certpilot.account.dto.GoalCertDtos.GoalResponse;
import com.OhRyue.certpilot.account.dto.LoginResponseDto;
import com.OhRyue.certpilot.account.dto.OnboardingDtos.OnboardingStatusResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.ResetPasswordRequest;
import com.OhRyue.certpilot.account.dto.SettingsDtos.SettingsResponse;
import com.OhRyue.certpilot.account.dto.TokenRefreshRequest;
import com.OhRyue.certpilot.account.dto.UserLoginDto;
import com.OhRyue.certpilot.account.dto.UserRegisterDto;
import com.OhRyue.certpilot.account.dto.UserResponseDto;
import com.OhRyue.certpilot.account.dto.VerifyCodeRequest;
import com.OhRyue.certpilot.account.dto.VerifyEmailRequest;
import com.OhRyue.certpilot.account.dto.WithdrawRequest;
import com.OhRyue.certpilot.account.dto.GoogleLoginRequest;
import com.OhRyue.certpilot.account.dto.KakaoLoginRequest;
import com.OhRyue.certpilot.account.dto.NaverLoginRequest;
import com.OhRyue.certpilot.account.dto.ConnectSocialAccountRequest;
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
import com.OhRyue.certpilot.account.service.EmailService;
import com.OhRyue.certpilot.account.service.GoalCertService;
import com.OhRyue.certpilot.account.service.OnboardingService;
import com.OhRyue.certpilot.account.service.ProfileService;
import com.OhRyue.certpilot.account.service.RefreshTokenService;
import com.OhRyue.certpilot.account.service.SettingsService;
import com.OhRyue.certpilot.account.service.SocialLoginService;
import com.OhRyue.certpilot.account.service.UserService;
import com.OhRyue.certpilot.account.service.VerificationCodeService;
import com.OhRyue.certpilot.account.feign.ProgressClient;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@Tag(name = "Account - Auth", description = "회원 인증 및 계정 조회 APIs")
@RestController
@RequestMapping("/api/account")
@RequiredArgsConstructor
public class AuthController {

  private static final Logger log = LoggerFactory.getLogger(AuthController.class);
  private final UserService userService;
  private final EmailService emailService;
  private final RefreshTokenService refreshTokenService;
  private final JwtTokenProvider jwtTokenProvider;
  private final VerificationCodeService verificationCodeService;
  private final ProfileService profileService;
  private final SettingsService settingsService;
  private final GoalCertService goalCertService;
  private final OnboardingService onboardingService;
  private final UserAccountRepository userAccountRepository;
  private final PasswordEncoder passwordEncoder;
  private final ProgressClient progressClient;
  private final SocialLoginService socialLoginService;

  /* ===================== 회원가입 & 이메일 인증 ===================== */

  @Operation(summary = "회원가입 - 이메일 인증코드 발송 (DB 저장 없음)")
  @PostMapping("/send-verification")
  public ResponseEntity<Map<String, String>> sendVerification(
      @Valid @RequestBody UserRegisterDto req
  ) {
    String userId = req.getUserId().trim();
    String email = normalizeEmail(req.getEmail());

    // 이메일 형식 검증
    if (email == null || email.isBlank() || !isValidEmailFormat(email)) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
          "message", "올바른 이메일 형식이 아닙니다."
      ));
    }

    // 이미 가입된 계정인지 체크
    if (userService.isUserIdDuplicate(userId) || userService.isEmailDuplicate(email)) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
          "message", "이미 가입된 계정입니다. 로그인 해주세요."
      ));
    }

    String code = String.format("%06d", (int) (Math.random() * 1_000_000));

    // 1) 코드 먼저 저장 (응답과 상관 없는 빠른 작업)
    verificationCodeService.saveCode(email, code);
    log.info("📝 [Auth] 인증코드 저장 완료 - email={}", email);

    // 2) 메일 발송은 비동기로 처리 (예외는 내부에서 로깅)
    log.info("📤 [Auth] 비동기 메일 발송 요청 - email={}", email);
    emailService.sendVerificationCodeAsync(email, code);

    // 3) 클라이언트에게는 "발송 요청 접수" 기준으로 빠르게 응답
    return ResponseEntity.ok(Map.of(
        "message", "인증코드 발송을 요청했습니다. 잠시 후 이메일을 확인해주세요."
    ));
  }

  @Operation(summary = "이메일 인증 + 회원가입 최종 완료 & 토큰 발급")
  @PostMapping("/verify-email")
  public ResponseEntity<LoginResponseDto> verifyEmail(
      @Valid @RequestBody VerifyEmailRequest req
  ) {
    String email = normalizeEmail(req.getEmail());
    String userId = req.getUserId().trim();

    String savedCode = verificationCodeService.getCode(email);
    if (savedCode == null || !savedCode.equals(req.getCode())) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(null);
    }

    verificationCodeService.deleteCode(email);

    if (userService.isUserIdDuplicate(userId) || userService.isEmailDuplicate(email)) {
      throw new IllegalArgumentException("이미 가입된 계정입니다. 로그인 해주세요.");
    }

    UserAccount user = userService.register(userId, email, req.getPassword());

    profileService.get(user.getId());
    settingsService.getSnapshot(user.getId());
    onboardingService.getStatus(user.getId());
    
    // 기본 인벤토리 초기화
    try {
      log.info("기본 인벤토리 초기화 시작: userId={}", user.getId());
      String result = progressClient.initializeDefaultInventory(user.getId());
      log.info("기본 인벤토리 초기화 완료: userId={}, result={}", user.getId(), result);
    } catch (Exception e) {
      log.error("기본 인벤토리 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
      // 회원가입은 성공했으므로 예외를 던지지 않음
    }

    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    // 회원가입 시점에는 온보딩이 완료되지 않았으므로 항상 false
    LoginResponseDto body = new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER",
        false
    );

    return ResponseEntity.ok(body);
  }

  /* ===================== 로그인 & 토큰 ===================== */

  @Operation(summary = "로그인 (userId + password) 및 토큰 발급")
  @PostMapping("/login")
  public ResponseEntity<LoginResponseDto> login(@Valid @RequestBody UserLoginDto req) {
    UserAccount user = userService.login(req.getUserId(), req.getPassword());

    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    UserProfile profile = profileService.get(user.getId());
    Boolean onboardingCompleted = profile.getOnboardingCompleted() != null ? profile.getOnboardingCompleted() : false;

    return ResponseEntity.ok(new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER",
        onboardingCompleted
    ));
  }

  @Operation(summary = "구글 소셜 로그인 및 토큰 발급")
  @PostMapping("/login/google")
  public ResponseEntity<LoginResponseDto> loginWithGoogle(@Valid @RequestBody GoogleLoginRequest req) {
    log.info("🔐 [Google Login] 구글 소셜 로그인 요청");
    
    // 구글 소셜 로그인 처리 (기존 계정 확인 및 신규 계정 생성)
    UserAccount user = socialLoginService.loginWithGoogle(req.getIdToken());
    
    // 신규 계정인 경우 프로필, 설정, 온보딩 초기화
    boolean isNewUser = user.getCreatedAt().equals(user.getLastLoginAt());
    if (isNewUser) {
      log.info("✨ [Google Login] 신규 사용자 - 프로필/설정/온보딩 초기화 - userId: {}", user.getId());
      try {
        profileService.get(user.getId());
        settingsService.getSnapshot(user.getId());
        onboardingService.getStatus(user.getId());
        
        // 기본 인벤토리 초기화
        try {
          log.info("기본 인벤토리 초기화 시작: userId={}", user.getId());
          String result = progressClient.initializeDefaultInventory(user.getId());
          log.info("기본 인벤토리 초기화 완료: userId={}, result={}", user.getId(), result);
        } catch (Exception e) {
          log.error("기본 인벤토리 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
        }
      } catch (Exception e) {
        log.error("프로필/설정/온보딩 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
      }
    }

    // JWT 토큰 발급
    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    UserProfile profile = profileService.get(user.getId());
    Boolean onboardingCompleted = profile.getOnboardingCompleted() != null ? profile.getOnboardingCompleted() : false;

    log.info("✅ [Google Login] 구글 소셜 로그인 완료 - userId: {}, email: {}", user.getId(), user.getEmail());
    
    return ResponseEntity.ok(new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER",
        onboardingCompleted
    ));
  }

  @Operation(summary = "카카오 소셜 로그인 및 토큰 발급")
  @PostMapping("/login/kakao")
  public ResponseEntity<LoginResponseDto> loginWithKakao(@Valid @RequestBody KakaoLoginRequest req) {
    log.info("🔐 [Kakao Login] 카카오 소셜 로그인 요청");
    
    UserAccount user = socialLoginService.loginWithKakao(req.getAccessToken());
    
    boolean isNewUser = user.getCreatedAt().equals(user.getLastLoginAt());
    if (isNewUser) {
      log.info("✨ [Kakao Login] 신규 사용자 - 프로필/설정/온보딩 초기화 - userId: {}", user.getId());
      try {
        profileService.get(user.getId());
        settingsService.getSnapshot(user.getId());
        onboardingService.getStatus(user.getId());
        
        try {
          log.info("기본 인벤토리 초기화 시작: userId={}", user.getId());
          String result = progressClient.initializeDefaultInventory(user.getId());
          log.info("기본 인벤토리 초기화 완료: userId={}, result={}", user.getId(), result);
        } catch (Exception e) {
          log.error("기본 인벤토리 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
        }
      } catch (Exception e) {
        log.error("프로필/설정/온보딩 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
      }
    }

    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    UserProfile profile = profileService.get(user.getId());
    Boolean onboardingCompleted = profile.getOnboardingCompleted() != null ? profile.getOnboardingCompleted() : false;

    log.info("✅ [Kakao Login] 카카오 소셜 로그인 완료 - userId: {}, email: {}", user.getId(), user.getEmail());
    
    return ResponseEntity.ok(new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER",
        onboardingCompleted
    ));
  }

  @Operation(summary = "네이버 소셜 로그인 및 토큰 발급")
  @PostMapping("/login/naver")
  public ResponseEntity<LoginResponseDto> loginWithNaver(@Valid @RequestBody NaverLoginRequest req) {
    log.info("🔐 [Naver Login] 네이버 소셜 로그인 요청");
    
    UserAccount user = socialLoginService.loginWithNaver(req.getCode(), req.getState());
    
    boolean isNewUser = user.getCreatedAt().equals(user.getLastLoginAt());
    if (isNewUser) {
      log.info("✨ [Naver Login] 신규 사용자 - 프로필/설정/온보딩 초기화 - userId: {}", user.getId());
      try {
        profileService.get(user.getId());
        settingsService.getSnapshot(user.getId());
        onboardingService.getStatus(user.getId());
        
        try {
          log.info("기본 인벤토리 초기화 시작: userId={}", user.getId());
          String result = progressClient.initializeDefaultInventory(user.getId());
          log.info("기본 인벤토리 초기화 완료: userId={}, result={}", user.getId(), result);
        } catch (Exception e) {
          log.error("기본 인벤토리 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
        }
      } catch (Exception e) {
        log.error("프로필/설정/온보딩 초기화 실패: userId={}, error={}", user.getId(), e.getMessage(), e);
      }
    }

    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
    refreshTokenService.save(user.getId(), refreshToken);

    UserProfile profile = profileService.get(user.getId());
    Boolean onboardingCompleted = profile.getOnboardingCompleted() != null ? profile.getOnboardingCompleted() : false;

    log.info("✅ [Naver Login] 네이버 소셜 로그인 완료 - userId: {}, email: {}", user.getId(), user.getEmail());
    
    return ResponseEntity.ok(new LoginResponseDto(
        accessToken,
        refreshToken,
        user.getId(),
        user.getEmail(),
        "USER",
        onboardingCompleted
    ));
  }

  @Operation(summary = "Access Token 재발급")
  @PostMapping("/refresh")
  public ResponseEntity<Map<String, String>> refresh(@Valid @RequestBody TokenRefreshRequest request) {
    String refreshToken = request.getRefreshToken();
    log.info("🔄 Access Token 재발급 요청 - Refresh Token 앞 20자: {}",
        refreshToken != null && refreshToken.length() > 20 ? refreshToken.substring(0, 20) + "..." : refreshToken);

    if (!jwtTokenProvider.validateToken(refreshToken)) {
      log.error("❌ Refresh Token 검증 실패");
      throw new IllegalArgumentException("리프레시 토큰이 유효하지 않습니다");
    }
    String userId = jwtTokenProvider.getUsernameFromToken(refreshToken);
    String savedToken = refreshTokenService.get(userId);
    if (!refreshToken.equals(savedToken)) {
      log.error("❌ Refresh Token 불일치 - userId: {}", userId);
      throw new IllegalArgumentException("리프레시 토큰이 일치하지 않습니다 (재로그인 필요)");
    }
    String newAccessToken = jwtTokenProvider.generateToken(userId);
    log.info("✅ Access Token 재발급 완료 - userId: {}", userId);
    return ResponseEntity.ok(Map.of("accessToken", newAccessToken));
  }

  @Operation(summary = "로그아웃 및 Refresh Token 제거")
  @PostMapping("/logout")
  public ResponseEntity<Void> logout() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    refreshTokenService.delete(userId);
    return ResponseEntity.noContent().build();
  }

  @Operation(summary = "계정 탈퇴")
  @DeleteMapping("/withdraw")
  public ResponseEntity<Map<String, String>> withdraw(@Valid @RequestBody WithdrawRequest req) {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    try {
      // 비밀번호 확인 후 계정 상태를 DELETED로 변경
      userService.withdraw(userId, req.getPassword());
      
      // Refresh Token 삭제
      refreshTokenService.delete(userId);
      
      log.info("✅ 계정 탈퇴 완료 - userId: {}", userId);
      return ResponseEntity.ok(Map.of(
          "message", "계정이 성공적으로 탈퇴되었습니다."
      ));
    } catch (IllegalArgumentException e) {
      log.warn("❌ 계정 탈퇴 실패 - userId: {}, reason: {}", userId, e.getMessage());
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(Map.of("message", e.getMessage()));
    } catch (IllegalStateException e) {
      log.warn("❌ 계정 탈퇴 실패 - userId: {}, reason: {}", userId, e.getMessage());
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(Map.of("message", e.getMessage()));
    }
  }

  /* ===================== 소셜 계정 연결 ===================== */

  @Operation(summary = "구글 소셜 계정 연결 (로그인 상태에서만 가능)")
  @PostMapping("/social/connect/google")
  public ResponseEntity<Map<String, String>> connectGoogleAccount(
      @Valid @RequestBody ConnectSocialAccountRequest req
  ) {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    try {
      socialLoginService.connectGoogleAccount(userId, req.getIdToken());
      log.info("✅ [Google Connect] 소셜 계정 연결 완료 - userId: {}", userId);
      return ResponseEntity.ok(Map.of(
          "message", "구글 소셜 계정이 성공적으로 연결되었습니다."
      ));
    } catch (IllegalArgumentException e) {
      log.warn("❌ [Google Connect] 소셜 계정 연결 실패 - userId: {}, reason: {}", userId, e.getMessage());
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(Map.of("message", e.getMessage()));
    } catch (Exception e) {
      log.error("❌ [Google Connect] 소셜 계정 연결 중 오류 발생 - userId: {}, error: {}", userId, e.getMessage(), e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(Map.of("message", "소셜 계정 연결 중 오류가 발생했습니다."));
    }
  }

  /* ===================== 아이디 중복 확인 ===================== */

  @Operation(summary = "아이디 중복 확인")
  @GetMapping("/check-userId")
  public ResponseEntity<Map<String, Object>> checkUserId(@RequestParam String userId) {
    boolean isDuplicate = userService.isUserIdDuplicate(userId);
    return ResponseEntity.ok(Map.of(
        "available", !isDuplicate,
        "message", isDuplicate ? "이미 존재하는 아이디입니다." : "사용 가능한 아이디입니다."
    ));
  }

  /* ===================== 닉네임 중복 확인 ===================== */

  @Operation(summary = "닉네임 중복 확인")
  @GetMapping("/check-nickname")
  public ResponseEntity<Map<String, Object>> checkNickname(@RequestParam String nickname) {
    boolean isDuplicate = profileService.isNicknameDuplicate(nickname);
    return ResponseEntity.ok(Map.of(
        "available", !isDuplicate,
        "message", isDuplicate ? "이미 존재하는 닉네임입니다." : "사용 가능한 닉네임입니다."
    ));
  }

  /* ===================== 비밀번호 찾기: 코드 발송/검증/재설정 ===================== */

  @Operation(summary = "비밀번호 찾기 - 인증코드 발송 (아이디 기준)")
  @PostMapping("/forgot-password")
  public ResponseEntity<Map<String, String>> sendResetCode(
      @Valid @RequestBody ForgotPasswordRequest dto
  ) {
    String userId = dto.getUserId().trim();
    UserAccount user = userService.findById(userId)
        .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 사용자입니다."));

    String email = normalizeEmail(user.getEmail());
    String code = verificationCodeService.generateResetCode(email);
    log.info("📤 [Auth] 비밀번호 찾기 - 비동기 메일 발송 요청 - email={}", email);
    emailService.sendVerificationCodeAsync(email, code);

    return ResponseEntity.ok(Map.of(
        "message", "인증 코드 발송을 요청했습니다. 잠시 후 이메일을 확인해주세요."
    ));
  }

  @Operation(summary = "비밀번호 찾기 - 인증코드 검증")
  @PostMapping("/forgot-password/verify")
  public ResponseEntity<Map<String, String>> verifyResetCode(
      @Valid @RequestBody VerifyCodeRequest dto
  ) {
    String email = normalizeEmail(dto.getEmail());
    boolean valid = verificationCodeService.verifyResetCode(email, dto.getCode());

    if (!valid) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
          "message", "코드가 유효하지 않거나 만료되었습니다."
      ));
    }

    return ResponseEntity.ok(Map.of(
        "message", "인증에 성공했습니다. 새 비밀번호를 설정해주세요."
    ));
  }

  @Operation(summary = "비밀번호 찾기 - 새 비밀번호 설정")
  @PostMapping("/forgot-password/reset")
  public ResponseEntity<Map<String, String>> resetPassword(
      @Valid @RequestBody ResetPasswordRequest dto
  ) {
    String email = normalizeEmail(dto.getEmail());

    UserAccount user = userService.findByEmail(email)
        .orElseThrow(() -> new IllegalArgumentException("해당 이메일의 사용자가 없습니다."));

    String encodedPassword = passwordEncoder.encode(dto.getNewPassword());
    user.setPasswordHash(encodedPassword);
    userAccountRepository.save(user);

    return ResponseEntity.ok(Map.of(
        "message", "비밀번호가 성공적으로 변경되었습니다."
    ));
  }

  /* ===================== 내 정보 조회 (/me) ===================== */

  @Operation(summary = "계정/프로필/설정/목표/온보딩 통합 조회")
  @GetMapping("/me")
  public ResponseEntity<MeResponse> me() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    UserAccount account = userService.findById(userId)
        .orElseThrow(() -> new IllegalStateException("사용자를 찾을 수 없습니다."));

    UserProfile profile = profileService.get(userId);
    SettingsResponse settings = settingsService.getSnapshot(userId);
    GoalResponse goal = goalCertService.getByUser(userId)
        .map(this::mapGoal)
        .orElse(null);
    OnboardingStatusResponse onboarding = onboardingService.getStatus(userId);

    // 온보딩 미완료 체크
    if (!onboarding.isCompleted()) {
      throw new com.OhRyue.certpilot.account.exception.OnboardingRequiredException();
    }

    MeResponse body = MeResponse.builder()
        .account(AccountSummary.builder()
            .userId(account.getId())
            .email(account.getEmail())
            .status(account.getStatus().name())
            .build())
        .profile(ProfileResponse.builder()
            .userId(profile.getUserId())
            .nickname(profile.getNickname())
            .skinId(profile.getSkinId())
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

  private boolean isValidEmailFormat(String email) {
    if (email == null || email.isBlank()) {
      return false;
    }
    // 기본적인 이메일 형식 검증 (RFC 5322의 간단한 버전)
    String emailRegex = "^[A-Za-z0-9+_.-]+@([A-Za-z0-9.-]+\\.[A-Za-z]{2,})$";
    return email.matches(emailRegex);
  }

  private GoalResponse mapGoal(UserGoalCert goal) {
    return GoalResponse.builder()
        .id(goal.getId())
        .userId(goal.getUserId())
        .certId(goal.getCertId())
        .targetExamMode(goal.getTargetExamMode())
        .targetRoundId(goal.getTargetRoundId())
        .targetExamDate(goal.getTargetExamDate())
        .ddayCached(goal.getDdayCached())
        .build();
  }
}
