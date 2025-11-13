package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.NotificationJob;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.domain.enums.NotificationChannel;
import com.OhRyue.certpilot.account.domain.enums.NotificationStatus;
import com.OhRyue.certpilot.account.domain.enums.NotificationType;
import com.OhRyue.certpilot.account.dto.SettingsDtos;
import com.OhRyue.certpilot.account.repo.NotificationJobRepository;
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
import com.OhRyue.certpilot.account.repo.UserSettingsRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class NotificationQueueService {

  private static final ZoneId KST = ZoneId.of("Asia/Seoul");

  private final NotificationJobRepository jobRepository;
  private final UserSettingsRepository userSettingsRepository;
  private final UserAccountRepository userAccountRepository;
  private final SettingsService settingsService;
  private final ObjectMapper objectMapper;

  @Transactional
  public void enqueueDailyGoals(LocalDate date) {
    Instant scheduleTime = date.atStartOfDay(KST).plusHours(8).toInstant();
    userSettingsRepository.findAll().forEach(settings -> {
      String userId = settings.getUserId();
      SettingsDtos.SettingsResponse pref = settingsService.getSnapshot(userId);
      SettingsDtos.NotificationPreferences notification = pref.getNotification();
      if (notification == null || !notification.isGoalReminderEnabled()) {
        return;
      }
      enqueueIfEnabled(userId, NotificationType.DAILY_GOAL, scheduleTime, notification);
    });
  }

  @Transactional
  public void enqueueWeeklySummary(LocalDate referenceMonday) {
    Instant scheduleTime = referenceMonday.atStartOfDay(KST).plusHours(9).toInstant();
    userSettingsRepository.findAll().forEach(settings -> {
      String userId = settings.getUserId();
      SettingsDtos.SettingsResponse pref = settingsService.getSnapshot(userId);
      SettingsDtos.NotificationPreferences notification = pref.getNotification();
      if (notification == null) {
        return;
      }
      if (notification.isEmailEnabled()) {
        enqueueJob(userId, NotificationChannel.EMAIL, NotificationType.WEEKLY_SUMMARY, scheduleTime, payloadForWeekly(userId));
      }
      if (notification.isPushEnabled()) {
        enqueueJob(userId, NotificationChannel.PUSH, NotificationType.WEEKLY_SUMMARY, scheduleTime, payloadForWeekly(userId));
      }
    });
  }

  private void enqueueIfEnabled(String userId,
                                NotificationType type,
                                Instant scheduleTime,
                                SettingsDtos.NotificationPreferences notification) {
    if (notification.isEmailEnabled()) {
      enqueueJob(userId, NotificationChannel.EMAIL, type, scheduleTime, payloadForDaily(userId));
    }
    if (notification.isPushEnabled()) {
      enqueueJob(userId, NotificationChannel.PUSH, type, scheduleTime, payloadForDaily(userId));
    }
  }

  private void enqueueJob(String userId,
                          NotificationChannel channel,
                          NotificationType type,
                          Instant scheduleTime,
                          String payloadJson) {
    if (jobRepository.existsByUserIdAndChannelAndTypeAndScheduledAt(userId, channel, type, scheduleTime)) {
      return;
    }
    NotificationJob job = new NotificationJob();
    job.setUserId(userId);
    job.setChannel(channel);
    job.setType(type);
    job.setScheduledAt(scheduleTime);
    job.setPayloadJson(payloadJson);
    job.setStatus(NotificationStatus.PENDING);
    jobRepository.save(job);
  }

  private String payloadForDaily(String userId) {
    Map<String, Object> payload = new HashMap<>();
    payload.put("subject", "오늘의 학습 목표를 달성해보세요!");
    payload.put("userId", userId);
    payload.put("type", "DAILY_GOAL");
    return writeJson(payload);
  }

  private String payloadForWeekly(String userId) {
    Map<String, Object> payload = new HashMap<>();
    payload.put("subject", "이번 주 학습 리포트를 확인하세요");
    payload.put("userId", userId);
    payload.put("type", "WEEKLY_SUMMARY");
    return writeJson(payload);
  }

  private String writeJson(Map<String, Object> payload) {
    try {
      return objectMapper.writeValueAsString(payload);
    } catch (JsonProcessingException e) {
      log.warn("Failed to serialize notification payload: {}", e.getMessage());
      return "{}";
    }
  }

  @Transactional
  public void dispatchPending(NotificationSender sender) {
    List<NotificationJob> jobs = jobRepository.findTop100ByStatusOrderByScheduledAtAsc(NotificationStatus.PENDING);
    for (NotificationJob job : jobs) {
      boolean sent = sender.send(job);
      if (sent) {
        job.setStatus(NotificationStatus.SENT);
        job.setSentAt(Instant.now());
      } else {
        job.setRetryCount(job.getRetryCount() + 1);
        job.setStatus(NotificationStatus.FAILED);
      }
    }
  }

  @Transactional(readOnly = true)
  public UserAccount findUser(String userId) {
    return userAccountRepository.findById(userId).orElse(null);
  }
}

