package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.dto.*;
import com.OhRyue.certpilot.account.service.RefreshTokenService;
import com.OhRyue.certpilot.account.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {

    private final UserService userService;                  // 사용자 관련 비즈니스 로직(회원가입, 로그인)을 처리하는 클래스
    private final JwtTokenProvider jwtTokenProvider;        // JWT 토큰 생성/검증 기능을 담당하는 클래스
    private final RefreshTokenService refreshTokenService;

    // 회원가입
    @PostMapping("/register")
    public ResponseEntity<UserResponseDto> register(@RequestBody UserRegisterDto req) {
        User user = userService.register(req.getUsername(), req.getPassword());
        UserResponseDto response = new UserResponseDto(
                "회원가입 성공",
                user.getId(),
                user.getUsername(),
                null // 회원가입 시 role 없으면 null
        );
        return ResponseEntity.ok(response);
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
}
