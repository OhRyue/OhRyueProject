package com.OhRyue.certpilot.progress.scheduler;

import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.service.NotificationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

/**
 * 일일 학습 알림 스케줄러
 * 매일 12:00 (Asia/Seoul)에 학습 알림을 발송합니다.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DailyReminderScheduler {

    private final NotificationService notificationService;
    private final AccountClient accountClient;

    @Scheduled(cron = "0 0 12 * * *", zone = "Asia/Seoul")
    public void sendDailyReminders() {
        try {
            log.info("Starting daily reminder notification job");
            List<AccountClient.UserSummary> users = accountClient.getUsersWithDailyReminderEnabled();
            log.info("Found {} users with daily reminder enabled", users.size());

            for (AccountClient.UserSummary user : users) {
                try {
                    notificationService.createNotification(
                            user.userId(),
                            NotificationType.DAILY_REMINDER,
                            "오늘도 학습하실 준비 되셨나요?",
                            "하루 10문제라도 좋으니, 지금 바로 CertPilot에서 학습을 시작해 보세요!",
                            Map.of()
                    );
                } catch (Exception e) {
                    log.error("Failed to send daily reminder to user {}: {}", user.userId(), e.getMessage(), e);
                }
            }

            log.info("Completed daily reminder notification job for {} users", users.size());
        } catch (Exception e) {
            log.error("Failed to send daily reminders: {}", e.getMessage(), e);
        }
    }
}




