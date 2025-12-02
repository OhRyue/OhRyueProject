package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.ProfileDtos.EquipSkinRequest;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileUpdateRequest;
import com.OhRyue.certpilot.account.service.ProfileService;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Account - Profile", description = "사용자 프로필 APIs")
@RestController
@RequestMapping("/api/account/profile")
@RequiredArgsConstructor
public class ProfileController {

  private final ProfileService profileService;
  private final com.OhRyue.certpilot.account.feign.ProgressClient progressClient;

  /* -------- 프로필 조회 -------- */
  @Operation(summary = "내 프로필 조회")
  @GetMapping
  public ResponseEntity<ProfileResponse> myProfile() {
    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    UserProfile profile = profileService.get(userId);
    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(profile.getUserId())
        .nickname(profile.getNickname())
        .skinId(profile.getSkinId())
        .timezone(profile.getTimezone())
        .lang(profile.getLang())
        .build());
  }

  /* -------- 프로필 수정 -------- */
  @Operation(summary = "내 프로필 갱신")
  @PutMapping
  public ResponseEntity<ProfileResponse> updateMyProfile(
      @RequestBody @Valid ProfileUpdateRequest req) {

    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    // 닉네임 중복 확인 (자기 자신의 닉네임은 제외)
    if (profileService.isNicknameDuplicate(req.getNickname(), userId)) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(null); // 중복된 닉네임
    }

    UserProfile existing = profileService.get(userId);
    UserProfile saved = profileService.upsert(
        UserProfile.builder()
            .userId(userId)
            .nickname(req.getNickname())
            .skinId(req.getSkinId() != null ? req.getSkinId() : existing.getSkinId())
            .timezone(req.getTimezone())
            .lang(req.getLang())
            .build()
    );
    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(saved.getUserId())
        .nickname(saved.getNickname())
        .skinId(saved.getSkinId())
        .timezone(saved.getTimezone())
        .lang(saved.getLang())
        .build());
  }

  /* -------- 스킨 장착 -------- */
  @Operation(summary = "스킨 장착")
  @PutMapping("/skin")
  public ResponseEntity<ProfileResponse> equipSkin(
      @RequestBody @Valid EquipSkinRequest req) {

    String userId;
    try {
      userId = AuthUserUtil.getCurrentUserId();
    } catch (IllegalStateException e) {
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    // 사용자가 해당 아이템을 보유하고 있는지 확인
    Boolean owned = progressClient.checkItemOwned(userId, req.getSkinId());
    if (owned == null || !owned) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST)
          .body(null); // 보유하지 않은 아이템
    }

    UserProfile profile = profileService.get(userId);
    profile.setSkinId(req.getSkinId());
    UserProfile saved = profileService.upsert(profile);

    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(saved.getUserId())
        .nickname(saved.getNickname())
        .skinId(saved.getSkinId())
        .timezone(saved.getTimezone())
        .lang(saved.getLang())
        .build());
  }
}
