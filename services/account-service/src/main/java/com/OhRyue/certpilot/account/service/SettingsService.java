package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.UserSettings;
import com.OhRyue.certpilot.account.dto.SettingsDtos;
import com.OhRyue.certpilot.account.repo.UserSettingsRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class SettingsService {

  private final UserSettingsRepository settingsRepository;
  private final ObjectMapper objectMapper;

  private static final SettingsDtos.UiPreferences DEFAULT_UI = SettingsDtos.UiPreferences.builder()
      .darkMode(false)
      .soundEffects(true)
      .build();

  private static final SettingsDtos.NotificationPreferences DEFAULT_NOTIFICATION = SettingsDtos.NotificationPreferences.builder()
      .pushEnabled(true)
      .emailEnabled(true)
      .goalReminderEnabled(false)
      .build();

  @Transactional
  public SettingsDtos.SettingsResponse getSnapshot(String userId) {
    UserSettings entity = ensureEntity(userId);
    SettingsDtos.UiPreferences ui = readUiPrefs(entity);
    SettingsDtos.NotificationPreferences notification = readNotificationPrefs(entity);
    return SettingsDtos.SettingsResponse.builder()
        .userId(userId)
        .ui(ui)
        .notification(notification)
        .build();
  }

  @Transactional
  public SettingsDtos.SettingsResponse update(String userId, SettingsDtos.SettingsUpdateRequest request) {
    UserSettings entity = ensureEntity(userId);
    SettingsDtos.UiPreferences currentUi = readUiPrefs(entity);
    SettingsDtos.NotificationPreferences currentNotif = readNotificationPrefs(entity);

    SettingsDtos.UiPreferences mergedUi = mergeUi(currentUi, request != null ? request.getUi() : null);
    SettingsDtos.NotificationPreferences mergedNotif = mergeNotification(currentNotif, request != null ? request.getNotification() : null);

    savePreferences(userId, mergedUi, mergedNotif);
    return SettingsDtos.SettingsResponse.builder()
        .userId(userId)
        .ui(mergedUi)
        .notification(mergedNotif)
        .build();
  }

  @Transactional
  public SettingsDtos.SettingsResponse reset(String userId) {
    savePreferences(userId, DEFAULT_UI, DEFAULT_NOTIFICATION);
    return SettingsDtos.SettingsResponse.builder()
        .userId(userId)
        .ui(DEFAULT_UI)
        .notification(DEFAULT_NOTIFICATION)
        .build();
  }

  private UserSettings ensureEntity(String userId) {
    return settingsRepository.findById(userId)
        .orElseGet(() -> settingsRepository.save(UserSettings.builder()
            .userId(userId)
            .uiPrefsJson(write(DEFAULT_UI))
            .notifPrefsJson(write(DEFAULT_NOTIFICATION))
            .build()));
  }

  private void savePreferences(String userId,
                               SettingsDtos.UiPreferences ui,
                               SettingsDtos.NotificationPreferences notification) {
    UserSettings entity = settingsRepository.findById(userId)
        .orElse(UserSettings.builder().userId(userId).build());
    entity.setUiPrefsJson(write(ui));
    entity.setNotifPrefsJson(write(notification));
    settingsRepository.save(entity);
  }

  private SettingsDtos.UiPreferences readUiPrefs(UserSettings entity) {
    return read(entity.getUiPrefsJson(), SettingsDtos.UiPreferences.class, DEFAULT_UI);
  }

  private SettingsDtos.NotificationPreferences readNotificationPrefs(UserSettings entity) {
    return read(entity.getNotifPrefsJson(), SettingsDtos.NotificationPreferences.class, DEFAULT_NOTIFICATION);
  }

  private SettingsDtos.UiPreferences mergeUi(SettingsDtos.UiPreferences current,
                                             SettingsDtos.UiPreferencesUpdate update) {
    if (update == null) {
      return current;
    }
    return SettingsDtos.UiPreferences.builder()
        .darkMode(update.getDarkMode() != null ? update.getDarkMode() : current.isDarkMode())
        .soundEffects(update.getSoundEffects() != null ? update.getSoundEffects() : current.isSoundEffects())
        .build();
  }

  private SettingsDtos.NotificationPreferences mergeNotification(SettingsDtos.NotificationPreferences current,
                                                                 SettingsDtos.NotificationPreferencesUpdate update) {
    if (update == null) {
      return current;
    }
    return SettingsDtos.NotificationPreferences.builder()
        .pushEnabled(update.getPushEnabled() != null ? update.getPushEnabled() : current.isPushEnabled())
        .emailEnabled(update.getEmailEnabled() != null ? update.getEmailEnabled() : current.isEmailEnabled())
        .goalReminderEnabled(update.getGoalReminderEnabled() != null ? update.getGoalReminderEnabled() : current.isGoalReminderEnabled())
        .build();
  }

  private <T> T read(String json, Class<T> type, T fallback) {
    if (json == null || json.isBlank()) {
      return fallback;
    }
    try {
      return objectMapper.readValue(json, type);
    } catch (JsonProcessingException ex) {
      log.warn("Failed to deserialize {} prefs, using fallback. payload={}", type.getSimpleName(), json, ex);
      return fallback;
    }
  }

  private String write(Object value) {
    try {
      return objectMapper.writeValueAsString(value);
    } catch (JsonProcessingException ex) {
      throw new IllegalStateException("환경설정 정보를 직렬화할 수 없습니다.", ex);
    }
  }
}
