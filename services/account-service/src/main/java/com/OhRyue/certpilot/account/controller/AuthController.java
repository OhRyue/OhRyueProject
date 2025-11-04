package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.dto.LoginResponseDto;
import com.OhRyue.certpilot.account.dto.UserLoginDto;
import com.OhRyue.certpilot.account.dto.UserRegisterDto;
import com.OhRyue.certpilot.account.dto.UserResponseDto;
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
        User user = userService.login(req.getUsername(), req.getPassword());

        // 로그인 성공하면 JWT 생성
        String token = jwtTokenProvider.generateToken(user.getUsername(), user.getRole());

        // DTO로 감싸서 반환
        LoginResponseDto response = new LoginResponseDto(
                "로그인 성공",
                token,
                user.getId(),
                user.getUsername(),
                user.getRole()
        );

        return ResponseEntity.ok(response);
    }

    // debug: 로그인한 사용자 정보 확인 API
    @GetMapping("/me")
    public ResponseEntity<?> getMyInfo(Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of(
                    "error", "unauthorized",
                    "message", "로그인이 필요합니다"
            ));
        }

        return ResponseEntity.ok(Map.of(
                "username", authentication.getName(),
                "authorities", authentication.getAuthorities()
        ));
    }
}
