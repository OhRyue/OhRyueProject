package com.OhRyue.certpilot.account.controller;

import com.OhRyue.certpilot.account.domain.UserProfile;
import com.OhRyue.certpilot.account.domain.UserSettings;
import com.OhRyue.certpilot.account.dto.GoalCertDtos.GoalResponse;
import com.OhRyue.certpilot.account.dto.ProfileDtos.ProfileResponse;
import com.OhRyue.certpilot.account.dto.SettingsDtos;
import com.OhRyue.certpilot.account.repo.UserSettingsRepository;
import com.OhRyue.certpilot.account.service.ProfileService;
import com.OhRyue.certpilot.account.service.GoalCertService;
import com.OhRyue.certpilot.account.service.SettingsService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Tag(name = "Account - Internal", description = "내부 서비스 전용 사용자 요약 API")
@RestController
@RequestMapping("/api/account/internal")
@RequiredArgsConstructor
public class AccountInternalController {

  private final ProfileService profileService;
  private final GoalCertService goalCertService;
  private final SettingsService settingsService;
  private final UserSettingsRepository userSettingsRepository;
  private final com.OhRyue.certpilot.account.repo.UserAccountRepository userAccountRepository;
  private final ObjectMapper objectMapper;

  @Operation(summary = "여러 사용자의 프로필 요약 조회")
  @GetMapping("/users/summaries")
  public ResponseEntity<List<ProfileResponse>> summaries(Authentication authentication,
                                                         @RequestParam(name = "ids") List<String> ids) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(401).build();
    }
    if (ids == null || ids.isEmpty()) {
      return ResponseEntity.ok(List.of());
    }

    List<UserProfile> profiles = profileService.getAll(ids);
    Map<String, ProfileResponse> responseMap = profiles.stream()
        .map(profile -> ProfileResponse.builder()
            .userId(profile.getUserId())
            .nickname(profile.getNickname())
            .skinId(profile.getSkinId())
            .timezone(profile.getTimezone())
            .lang(profile.getLang())
            .build())
        .collect(Collectors.toMap(ProfileResponse::getUserId, resp -> resp, (a, b) -> a, LinkedHashMap::new));

    List<ProfileResponse> ordered = ids.stream()
        .map(id -> responseMap.getOrDefault(id,
            ProfileResponse.builder()
                .userId(id)
                .nickname(null)
                .skinId(null)
                .timezone(null)
                .lang(null)
                .build()))
        .collect(Collectors.toList());

    return ResponseEntity.ok(ordered);
  }

  @Operation(summary = "사용자 목표 자격증 조회(내부 전용)")
  @GetMapping("/users/goal")
  public ResponseEntity<GoalResponse> goal(@RequestParam String userId) {
    return goalCertService.getByUser(userId)
        .map(goal -> GoalResponse.builder()
            .id(goal.getId())
            .userId(goal.getUserId())
            .certId(goal.getCertId())
            .targetExamMode(goal.getTargetExamMode())
            .targetRoundId(goal.getTargetRoundId())
            .ddayCached(goal.getDdayCached())
            .build())
        .map(ResponseEntity::ok)
        .orElse(ResponseEntity.noContent().build());
  }

  @Operation(summary = "일일 학습 알림을 받을 사용자 목록 조회(내부 전용)")
  @GetMapping("/users/daily-reminder-enabled")
  public ResponseEntity<List<UserSummary>> getUsersWithDailyReminderEnabled(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(401).build();
    }

    List<UserSummary> users = userSettingsRepository.findAll().stream()
        .map(settings -> {
          SettingsDtos.SettingsResponse snapshot = settingsService.getSnapshot(settings.getUserId());
          SettingsDtos.NotificationPreferences notification = snapshot.getNotification();
          if (notification != null && notification.isGoalReminderEnabled()) {
            return new UserSummary(settings.getUserId());
          }
          return null;
        })
        .filter(user -> user != null)
        .collect(Collectors.toList());

    return ResponseEntity.ok(users);
  }

  @Operation(summary = "주간 리포트를 받을 사용자 목록 조회(내부 전용)")
  @GetMapping("/users/weekly-report-enabled")
  public ResponseEntity<List<UserWithEmail>> getUsersWithWeeklyReportEnabled(Authentication authentication) {
    if (authentication == null || !authentication.isAuthenticated()) {
      return ResponseEntity.status(401).build();
    }

    List<UserWithEmail> users = userSettingsRepository.findAll().stream()
        .map(settings -> {
          SettingsDtos.SettingsResponse snapshot = settingsService.getSnapshot(settings.getUserId());
          SettingsDtos.NotificationPreferences notification = snapshot.getNotification();
          if (notification != null && notification.isEmailEnabled()) {
            return userAccountRepository.findById(settings.getUserId())
                .map(account -> new UserWithEmail(account.getId(), account.getEmail()))
                .orElse(null);
          }
          return null;
        })
        .filter(user -> user != null)
        .collect(Collectors.toList());

    return ResponseEntity.ok(users);
  }

  public record UserSummary(String userId) {}
  public record UserWithEmail(String userId, String email) {}
}

