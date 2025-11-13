package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.dto.SettingsDtos.SettingsResponse;
import com.OhRyue.certpilot.account.dto.SettingsDtos.SettingsUpdateRequest;
import com.OhRyue.certpilot.account.service.SettingsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Account - Settings", description = "사용자 환경설정 APIs")
@RestController
@RequestMapping("/api/account/settings")
@RequiredArgsConstructor
public class SettingsController {

  private final SettingsService settingsService;

  /* -------- 환경설정 조회 -------- */
  @Operation(summary = "내 환경설정 조회")
  @GetMapping
  public ResponseEntity<SettingsResponse> mySettings(Authentication auth) {
    String userId = auth.getName();
    return ResponseEntity.ok(settingsService.getSnapshot(userId));
  }

  /* -------- 환경설정 업데이트 -------- */
  @Operation(summary = "내 환경설정 저장")
  @PutMapping
  public ResponseEntity<SettingsResponse> update(Authentication auth,
                                                 @RequestBody @Valid SettingsUpdateRequest req) {
    String userId = auth.getName();
    return ResponseEntity.ok(settingsService.update(userId, req));
  }

  /* -------- 환경설정 초기화 -------- */
  @Operation(summary = "환경설정 초기화", description = "다크모드/효과음/알림 설정을 기본값으로 되돌립니다.")
  @PostMapping("/reset")
  public ResponseEntity<SettingsResponse> reset(Authentication auth) {
    String userId = auth.getName();
    return ResponseEntity.ok(settingsService.reset(userId));
  }
}
