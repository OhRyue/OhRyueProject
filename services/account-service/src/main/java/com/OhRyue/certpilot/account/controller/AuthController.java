package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.dto.UserLoginDto;
import com.OhRyue.certpilot.account.dto.UserRegisterDto;
import com.OhRyue.certpilot.account.dto.UserResponseDto;
import com.OhRyue.certpilot.account.service.UserService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private final UserService userService;

    public AuthController(UserService userService) {
        this.userService = userService;
    }

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
    public ResponseEntity<UserResponseDto> login(@RequestBody UserLoginDto req) {
        User user = userService.login(req.getUsername(), req.getPassword());
        UserResponseDto response = new UserResponseDto(
                "로그인 성공",
                user.getId(),
                user.getUsername(),
                user.getRole()
        );
        return ResponseEntity.ok(response);
    }
}
