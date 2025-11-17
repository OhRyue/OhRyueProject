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
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
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
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
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
    private final UserAccountRepository userAccountRepository;
    private final PasswordEncoder passwordEncoder;

    /* ===================== 회원가입 & 이메일 인증 ===================== */

    /**
     * 회원가입 폼 제출 시:
     * - userId / email / password를 받고
     * - userId/email 중복이면 "이미 가입된 계정" 안내
     * - DB에 저장하지 않고, 이메일로 인증코드만 발송
     */
    @Operation(summary = "회원가입 - 이메일 인증코드 발송 (DB 저장 없음)")
    @PostMapping("/send-verification")
    public ResponseEntity<Map<String, String>> sendVerification(
            @Valid @RequestBody UserRegisterDto req, HttpServletRequest request
    ) {
        System.out.println("REQ URI = " + request.getRequestURI());     // 디버깅용
        String userId = req.getUserId().trim();
        String email = normalizeEmail(req.getEmail());

        // 이미 가입된 계정인지 체크
        if (userService.isUserIdDuplicate(userId) || userService.isEmailDuplicate(email)) {
            // 요구사항: "이미 있는 정보니 로그인하라고"
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                    "message", "이미 가입된 계정입니다. 로그인 해주세요."
            ));
        }

        // 인증코드 생성 및 저장
        String code = String.format("%06d", (int) (Math.random() * 1_000_000));
        verificationCodeService.saveCode(email, code);

        // 이메일 발송
        emailService.sendVerificationCode(email, code);

        return ResponseEntity.ok(Map.of(
                "message", "인증코드가 이메일로 전송되었습니다."
        ));
    }

    /**
     * 이메일 + 인증코드 + userId + password를 받아 최종 가입 처리
     * - 이 시점에서 UserAccount를 생성하고 ACTIVE로 저장
     * - 바로 Access/Refresh 토큰 발급 → 프론트는 즉시 로그인 상태에서 온보딩 분기
     */
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
                    .body(null); // GlobalExceptionHandler를 쓰지 않는 간단 버전
        }

        // 인증코드는 일회용으로 삭제
        verificationCodeService.deleteCode(email);

        // 혹시라도 이미 가입되어 있다면 → 로그인하라고 안내 (중복 방지)
        if (userService.isUserIdDuplicate(userId) || userService.isEmailDuplicate(email)) {
            // 여기서는 토큰을 발급하지 않고, 프론트에서 로그인 플로우로 유도
            throw new IllegalArgumentException("이미 가입된 계정입니다. 로그인 해주세요.");
        }

        // 실제 UserAccount 생성 (ACTIVE)
        UserAccount user = userService.register(userId, email, req.getPassword());

        // 기본 프로필/설정/온보딩 스냅샷 초기화
        profileService.get(user.getId());
        settingsService.getSnapshot(user.getId());
        onboardingService.getStatus(user.getId());

        // 바로 토큰 발급 → 가입 직후 로그인 상태
        String accessToken = jwtTokenProvider.generateToken(user.getId());
        String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());
        refreshTokenService.save(user.getId(), refreshToken);

        LoginResponseDto body = new LoginResponseDto(
                accessToken,
                refreshToken,
                user.getId(),
                user.getEmail(),
                "USER"
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
        String userId = jwtTokenProvider.getUsernameFromToken(refreshToken);
        String savedToken = refreshTokenService.get(userId);
        if (!refreshToken.equals(savedToken)) {
            throw new IllegalArgumentException("리프레시 토큰이 일치하지 않습니다 (재로그인 필요)");
        }
        String newAccessToken = jwtTokenProvider.generateToken(userId);
        return ResponseEntity.ok(Map.of("accessToken", newAccessToken));
    }

    @Operation(summary = "로그아웃 및 Refresh Token 제거")
    @PostMapping("/logout")
    public ResponseEntity<Void> logout(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        String userId = authentication.getName();
        refreshTokenService.delete(userId);
        return ResponseEntity.noContent().build();
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
        emailService.sendVerificationCode(email, code);

        return ResponseEntity.ok(Map.of(
                "message", "인증 코드가 이메일로 전송되었습니다."
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
