package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.User;
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

    /**
     * 회원가입 API
     * 요청 예시:
     * {
     *   "username": "testuser",
     *   "password": "1234"
     * }
     */
    @PostMapping("/register")
    public ResponseEntity<?> register(@RequestBody Map<String, String> body) {
        String username = body.get("username");
        String password = body.get("password");

        try {
            User user = userService.register(username, password);
            return ResponseEntity.ok(Map.of(
                    "message", "회원가입 성공",
                    "userId", user.getId(),
                    "username", user.getUsername()
            ));
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(Map.of("error", e.getMessage()));
        }
    }

    /**
     * 로그인 API
     * 요청 예시:
     * {
     *   "username": "testuser",
     *   "password": "1234"
     * }
     */
    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody Map<String, String> body) {
        String username = body.get("username");
        String password = body.get("password");

        try {
            User user = userService.login(username, password);
            return ResponseEntity.ok(Map.of(
                    "message", "로그인 성공",
                    "userId", user.getId(),
                    "username", user.getUsername(),
                    "role", user.getRole()
            ));
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(Map.of("error", e.getMessage()));
        }
    }
}
