package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.dto.*;
import com.OhRyue.certpilot.account.repo.UserRepository;
import com.OhRyue.certpilot.account.service.EmailService;
import com.OhRyue.certpilot.account.service.RefreshTokenService;
import com.OhRyue.certpilot.account.service.UserService;
import com.OhRyue.certpilot.account.service.VerificationCodeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/account")
@RequiredArgsConstructor
public class AuthController {

    private final UserService userService;                  // 사용자 관련 비즈니스 로직(회원가입, 로그인)을 처리하는 클래스
    private final EmailService emailService;
    private final RefreshTokenService refreshTokenService;
    private final VerificationCodeService verificationCodeService;
    private final UserRepository userRepository;
    private final JwtTokenProvider jwtTokenProvider;        // JWT 토큰 생성/검증 기능을 담당하는 클래스
    private final PasswordEncoder passwordEncoder;

    // 회원가입
    // 인증 메일 전송 (DB 저장 안 함)
    @PostMapping("/send-verification")
    public ResponseEntity<?> sendVerification(@Valid @RequestBody UserRegisterDto req) {

        // 아이디 중복 체크
        if (userService.isUsernameDuplicate(req.getUsername())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                    "message", "이미 존재하는 아이디입니다."
            ));
        }

        // 인증코드 생성
        String verificationCode = String.format("%06d", (int) (Math.random() * 1000000));

        // Redis 저장 (10분 유효)
        verificationCodeService.saveCode(req.getEmail(), verificationCode);

        // 이메일 발송
        emailService.sendVerificationCode(req.getEmail(), verificationCode);

        return ResponseEntity.ok(Map.of(
                "message", "인증코드가 이메일로 전송되었습니다."
        ));
    }

    // 인증 + 회원가입(DB 저장)
    @PostMapping("/verify-email")
    public ResponseEntity<?> verifyEmail(@RequestBody VerifyEmailRequest req) {
        String savedCode = verificationCodeService.getCode(req.getEmail());

        if (savedCode == null || !savedCode.equals(req.getCode())) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of(
                    "message", "인증 코드가 틀리거나 만료되었습니다"
            ));
        }

        // 인증 성공 → 유저 DB 저장
        User user = userService.register(req.getUsername(), req.getPassword(), req.getEmail());

        // Redis 코드 삭제
        verificationCodeService.deleteCode(req.getEmail());

        return ResponseEntity.ok(Map.of(
                "message", "이메일 인증 완료! 회원가입이 완료되었습니다.",
                "username", user.getUsername()
        ));
    }


    // 로그인
    @PostMapping("/login")
    public ResponseEntity<LoginResponseDto> login(@RequestBody UserLoginDto req) {
        // 1) ID & PW 확인
        User user = userService.login(req.getUsername(), req.getPassword());

        // 2) Access Token & Refresh Token 생성
        String accessToken = jwtTokenProvider.generateToken(user.getUsername(), user.getRole());
        String refreshToken = jwtTokenProvider.generateRefreshToken(user.getUsername(), user.getRole());

        // 3) Refresh Token을 Redis에 저장 (username = key)
        refreshTokenService.save(user.getUsername(), refreshToken);

        // 4) 응답 DTO로 반환
        LoginResponseDto response = new LoginResponseDto(
                "로그인 성공",
                accessToken,
                refreshToken,
                user.getId(),
                user.getUsername(),
                user.getRole()
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
        User user = userService.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("사용자를 찾을 수 없습니다"));

        return ResponseEntity.ok(Map.of(
                "id", user.getId(),
                "username", user.getUsername(),
                "role", user.getRole()
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
        String role = jwtTokenProvider.getRoleFromToken(refreshToken);

        // 3) Redis에도 저장돼있는지 확인
        String savedToken = refreshTokenService.get(username);
        if (!refreshToken.equals(savedToken)) {
            throw new IllegalArgumentException("리프레시 토큰이 일치하지 않습니다 (재로그인 필요)");
        }

        // 4) 새로운 Access Token 발급
        String newAccessToken = jwtTokenProvider.generateToken(username, role);

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

    // 아이디 중복 확인
    @GetMapping("/check-username")
    public ResponseEntity<?> checkUsername(@RequestParam String username) {
        boolean isDuplicate = userService.isUsernameDuplicate(username);

        return ResponseEntity.ok(Map.of(
                "available", !isDuplicate,  // 프론트에서 res.data.available로 받음
                "message", isDuplicate ? "이미 존재하는 아이디입니다." : "사용 가능한 아이디입니다."
        ));
    }

    // 비밀번호 찾기: 인증 코드 보내기
    @PostMapping("/forgot-password")
    public ResponseEntity<String> sendVerificationCode(@RequestBody ForgotPasswordRequest dto) {
        User user = userRepository.findByUsername(dto.getUsername())
                .orElseThrow(() -> new RuntimeException("유저를 찾을 수 없습니다."));

        String code = verificationCodeService.generateCode(user.getEmail());
        emailService.sendVerificationCode(user.getEmail(), code);

        return ResponseEntity.ok("인증 코드가 이메일로 전송되었습니다.");
    }

    // 비밀번호 찾기: 인증 코드 확인
    @PostMapping("/verify-code")
    public ResponseEntity<String> verifyCode(@RequestBody VerifyCodeRequest dto) {
        boolean isValid = verificationCodeService.verify(dto.getEmail(), dto.getCode());
        if (!isValid) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("코드가 유효하지 않습니다.");
        }
        return ResponseEntity.ok("인증 성공");
    }

    // 비밀번호 찾기: 새 비밀번호 설정
    @PostMapping("/reset-password")
    public ResponseEntity<String> resetPassword(@RequestBody ResetPasswordRequest dto) {
        User user = userRepository.findByEmail(dto.getEmail())
                .orElseThrow(() -> new RuntimeException("유저를 찾을 수 없습니다."));
        String encodedPassword = passwordEncoder.encode(dto.getNewPassword());
        user.setPassword(encodedPassword);
        userRepository.save(user);

        return ResponseEntity.ok("비밀번호가 성공적으로 변경되었습니다.");
    }

}
