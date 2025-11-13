package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileUpdateRequest;
import com.OhRyue.certpilot.account.service.ProfileService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Account - Profile", description = "사용자 프로필 APIs")
@RestController
@RequestMapping("/api/account/profile")
@RequiredArgsConstructor
public class ProfileController {

  private final ProfileService profileService;

  /* -------- 프로필 조회 -------- */
  @Operation(summary = "내 프로필 조회")
  @GetMapping
  public ResponseEntity<ProfileResponse> myProfile(Authentication auth) {
    String userId = auth.getName();
    UserProfile profile = profileService.get(userId);
    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(profile.getUserId())
        .nickname(profile.getNickname())
        .avatarUrl(profile.getAvatarUrl())
        .timezone(profile.getTimezone())
        .lang(profile.getLang())
        .build());
  }

  /* -------- 프로필 수정 -------- */
  @Operation(summary = "내 프로필 갱신")
  @PutMapping
  public ResponseEntity<ProfileResponse> updateMyProfile(Authentication auth,
                                                         @RequestBody @Valid ProfileUpdateRequest req) {
    String userId = auth.getName();
    UserProfile saved = profileService.upsert(
        UserProfile.builder()
            .userId(userId)
            .nickname(req.getNickname())
            .avatarUrl(req.getAvatarUrl())
            .timezone(req.getTimezone())
            .lang(req.getLang())
            .build()
    );
    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(saved.getUserId())
        .nickname(saved.getNickname())
        .avatarUrl(saved.getAvatarUrl())
        .timezone(saved.getTimezone())
        .lang(saved.getLang())
        .build());
  }
}
