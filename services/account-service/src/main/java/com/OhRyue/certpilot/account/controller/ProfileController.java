package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.dto.ProfileDtos.*;
import com.OhRyue.certpilot.account.service.ProfileService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/account/profile")
@RequiredArgsConstructor
public class ProfileController {

  private final ProfileService profileService;

  @GetMapping("/me")
  public ResponseEntity<ProfileResponse> myProfile(Authentication auth) {
    String userId = auth.getName();
    UserProfile p = profileService.get(userId);
    return ResponseEntity.ok(ProfileResponse.builder()
        .userId(p.getUserId())
        .nickname(p.getNickname())
        .avatarUrl(p.getAvatarUrl())
        .timezone(p.getTimezone())
        .lang(p.getLang())
        .build());
  }

  @PutMapping("/me")
  public ResponseEntity<ProfileResponse> updateMyProfile(Authentication auth,
                                                         @RequestBody ProfileUpdateRequest req) {
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
