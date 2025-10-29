package com.OhRyue.certpilot.auth.web;

import com.OhRyue.certpilot.auth.dto.LoginRequest;
import com.OhRyue.certpilot.auth.dto.UserDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Auth", description = "인증 API (Stub)")
@RestController
@RequestMapping(path = "/api/auth", produces = MediaType.APPLICATION_JSON_VALUE)
public class AuthController {

  @Operation(summary = "로그인", description = "Stub: 항상 성공, 데모 사용자 반환")
  @PostMapping("/login")
  public UserDto login(@RequestBody LoginRequest request) {
    // TODO: Implement real authentication
    return new UserDto(1L, request.getUsername(), request.getUsername() + "@demo.local");
  }

  @Operation(summary = "현재 사용자 정보", description = "Stub: 데모 사용자 반환")
  @GetMapping("/me")
  public UserDto me() {
    // TODO: Extract from JWT token
    return new UserDto(1L, "demo", "demo@local");
  }
}

