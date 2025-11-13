package com.OhRyue.certpilot.account.service;

import com.OhRyue.certpilot.account.domain.NotificationJob;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.domain.enums.NotificationChannel;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class NotificationSender {

  private final NotificationQueueService queueService;

  public boolean send(NotificationJob job) {
    UserAccount account = queueService.findUser(job.getUserId());
    if (account == null || !account.isActive()) {
      log.debug("Skipping notification for inactive user {}", job.getUserId());
      return true;
    }
    if (job.getChannel() == NotificationChannel.EMAIL) {
      return sendEmail(account.getEmail(), job.getPayloadJson());
    } else if (job.getChannel() == NotificationChannel.PUSH) {
      return sendPush(account.getId(), job.getPayloadJson());
    }
    return false;
  }

  private boolean sendEmail(String email, String payload) {
    log.info("[NOTIFY][EMAIL] to={} payload={}", email, payload);
    return true;
  }

  private boolean sendPush(String userId, String payload) {
    log.info("[NOTIFY][PUSH] userId={} payload={}", userId, payload);
    return true;
  }
}

