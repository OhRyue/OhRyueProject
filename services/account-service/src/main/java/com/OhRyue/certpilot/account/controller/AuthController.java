package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.dto.*;
import com.OhRyue.certpilot.account.service.EmailService;
import com.OhRyue.certpilot.account.service.RefreshTokenService;
import com.OhRyue.certpilot.account.service.UserService;
import com.OhRyue.certpilot.account.service.VerificationCodeService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/account")
@RequiredArgsConstructor
public class AuthController {

  private final UserService userService;                  // 사용자 관련 비즈니스 로직(회원가입, 로그인)을 처리하는 클래스
  private final EmailService emailService;
  private final RefreshTokenService refreshTokenService;
  private final JwtTokenProvider jwtTokenProvider;        // JWT 토큰 생성/검증 기능을 담당하는 클래스
  private final VerificationCodeService verificationCodeService;

  // 회원가입
  @PostMapping("/register")
  public ResponseEntity<UserResponseDto> register(@RequestBody UserRegisterDto req) {
    // 1) 유저 저장 (status = BLOCKED로 저장)
    UserAccount user = userService.register(req.getUsername(), req.getPassword(), req.getEmail());

    // 2) 6자리 인증코드 생성
    String verificationCode = String.format("%06d", (int) (Math.random() * 1000000));

    // 3) Redis에 VC:email 형태로 저장 (10분 유효)
    verificationCodeService.saveCode(user.getEmail(), verificationCode);

    // 4) 이메일 발송
    emailService.sendVerificationCode(user.getEmail(), verificationCode);

    // 5) 응답 반환
    return ResponseEntity.ok(
        new UserResponseDto(
            "회원가입 완료! 이메일 인증을 진행해주세요.",
            user.getId(),
            user.getId(),
            "USER"
        )
    );
  }

  // 인증
  @PostMapping("/verify-email")
  public ResponseEntity<?> verifyEmail(@RequestBody VerifyEmailRequest request) {
    String savedCode = verificationCodeService.getCode(request.getEmail());

    if (savedCode == null || !savedCode.equals(request.getCode())) {
      return ResponseEntity.status(400).body("인증 코드가 틀리거나 만료되었습니다");
    }

    // 이메일 인증 성공 → 이메일 기준으로 활성화 처리
    userService.enableUser(request.getEmail());
    verificationCodeService.deleteCode(request.getEmail());

    return ResponseEntity.ok("이메일 인증 완료!");
  }

  // 로그인
  @PostMapping("/login")
  public ResponseEntity<LoginResponseDto> login(@RequestBody UserLoginDto req) {
    // 1) ID & PW 확인
    UserAccount user = userService.login(req.getUsername(), req.getPassword());

    // 2) Access Token & Refresh Token 생성 (role=USER 고정)
    String accessToken = jwtTokenProvider.generateToken(user.getId());
    String refreshToken = jwtTokenProvider.generateRefreshToken(user.getId());

    // 3) Refresh Token을 Redis에 저장 (username = key)
    refreshTokenService.save(user.getId(), refreshToken);

    // 4) 응답 DTO로 반환
    LoginResponseDto response = new LoginResponseDto(
        "로그인 성공",
        accessToken,
        refreshToken,
        user.getId(),
        user.getId(),
        "USER"
    );

    return ResponseEntity.ok(response);
  }

  // JWT을 기반으로 DB에서 유저 데이터 조회해서 반환
  @GetMapping("/me")
  public ResponseEntity<?> getMyInfo(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
          "error", "unauthorized",
          "message", "로그인이 필요합니다"
      ));
    }

    String username = authentication.getName();
    UserAccount user = userService.findByUsername(username)
        .orElseThrow(() -> new RuntimeException("사용자를 찾을 수 없습니다"));

    return ResponseEntity.ok(Map.of(
        "id", user.getId(),
        "username", user.getId(),
        "role", "USER"
    ));
  }

  // Refresh Token을 검증하고 새로운 Access Token을 발급하는 API
  @PostMapping("/refresh")
  public ResponseEntity<?> refresh(@RequestBody TokenRefreshRequest request) {
    String refreshToken = request.getRefreshToken();

    // 1) 유효한 토큰인지 확인
    if (!jwtTokenProvider.validateToken(refreshToken)) {
      throw new IllegalArgumentException("리프레시 토큰이 유효하지 않습니다");
    }

    // 2) 토큰에서 사용자 정보 추출
    String username = jwtTokenProvider.getUsernameFromToken(refreshToken);

    // 3) Redis에도 저장돼있는지 확인
    String savedToken = refreshTokenService.get(username);
    if (!refreshToken.equals(savedToken)) {
      throw new IllegalArgumentException("리프레시 토큰이 일치하지 않습니다 (재로그인 필요)");
    }

    // 4) 새로운 Access Token 발급
    String newAccessToken = jwtTokenProvider.generateToken(username);

    return ResponseEntity.ok(Map.of(
        "message", "AccessToken 재발급 성공",
        "accessToken", newAccessToken
    ));
  }

  // 로그아웃
  @PostMapping("/logout")
  public ResponseEntity<?> logout(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
          "error", "unauthorized",
          "message", "로그인이 필요합니다"
      ));
    }

    String username = authentication.getName();

    // Redis에서 Refresh Token 삭제
    refreshTokenService.delete(username);

    return ResponseEntity.ok(Map.of(
        "message", "로그아웃 성공",
        "username", username
    ));
  }
}
