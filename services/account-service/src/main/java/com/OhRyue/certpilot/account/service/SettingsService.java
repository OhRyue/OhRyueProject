package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserSettings;
import com.OhRyue.certpilot.account.repo.UserSettingsRepository;
import org.springframework.stereotype.Service;

@Service
public class SettingsService {

  private final UserSettingsRepository settingsRepository;

  public SettingsService(UserSettingsRepository settingsRepository) {
    this.settingsRepository = settingsRepository;
  }

  public UserSettings get(String userId) {
    return settingsRepository.findById(userId)
        .orElseGet(() -> UserSettings.builder()
            .userId(userId)
            .uiPrefsJson("{\"theme\":\"light\",\"sound\":{\"correct\":true,\"wrong\":true}}")
            .notifPrefsJson("{\"dailyReminder\":{\"enabled\":false,\"hhmm\":\"12:00\"},\"weeklyReport\":{\"enabled\":false,\"email\":null}}")
            .build());
  }

  public UserSettings upsert(UserSettings settings) {
    return settingsRepository.save(settings);
  }
}
