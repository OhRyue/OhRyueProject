package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.User;
import com.OhRyue.certpilot.account.dto.UserLoginDto;
import com.OhRyue.certpilot.account.dto.UserRegisterDto;
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

    @PostMapping("/register")
    public ResponseEntity<?> register(@RequestBody UserRegisterDto req) {
        User user = userService.register(req.getUsername(), req.getPassword());
        return ResponseEntity.ok(Map.of("message","회원가입 성공","userId",user.getId(),"username",user.getUsername()));
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody UserLoginDto req) {
        User user = userService.login(req.getUsername(), req.getPassword());
        return ResponseEntity.ok(Map.of("message","로그인 성공","userId",user.getId(),"username",user.getUsername(),"role",user.getRole()));
    }
}
