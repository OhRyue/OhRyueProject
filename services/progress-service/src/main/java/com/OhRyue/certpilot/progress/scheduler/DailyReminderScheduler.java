package com.OhRyue.certpilot.progress.scheduler;

import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.service.NotificationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
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

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final NotificationService notificationService;
    private final AccountClient accountClient;

    @Scheduled(cron = "0 0 12 * * *", zone = "Asia/Seoul")
    public void sendDailyReminders() {
        LocalDateTime now = LocalDateTime.now(KST);
        java.time.ZoneId systemDefault = java.time.ZoneId.systemDefault();
        
        log.info("========================================");
        log.info("📅 [DailyReminderScheduler] 스케줄러 실행 시작");
        log.info("   현재 시간 (Asia/Seoul): {}", now.format(FORMATTER));
        log.info("   시스템 기본 타임존: {}", systemDefault);
        log.info("   JVM 타임존: {}", System.getProperty("user.timezone", "미설정"));
        log.info("   환경변수 TZ: {}", System.getenv("TZ"));
        log.info("========================================");
        
        try {
            List<AccountClient.UserSummary> users = accountClient.getUsersWithDailyReminderEnabled();
            log.info("✅ [DailyReminderScheduler] 일일 학습 알림을 받을 사용자 수: {}", users.size());

            if (users.isEmpty()) {
                log.warn("⚠️ [DailyReminderScheduler] 일일 학습 알림을 받을 사용자가 없습니다.");
                return;
            }

            int successCount = 0;
            int failCount = 0;

            for (AccountClient.UserSummary user : users) {
                try {
                    notificationService.createNotification(
                            user.userId(),
                            NotificationType.DAILY_REMINDER,
                            "오늘도 학습하실 준비 되셨나요?",
                            "하루 10문제라도 좋으니, 지금 바로 CertPilot에서 학습을 시작해 보세요!",
                            Map.of()
                    );
                    successCount++;
                    log.debug("✅ [DailyReminderScheduler] 사용자 {}에게 일일 학습 알림 발송 성공", user.userId());
                } catch (Exception e) {
                    failCount++;
                    log.error("❌ [DailyReminderScheduler] 사용자 {}에게 일일 학습 알림 발송 실패: {}", 
                            user.userId(), e.getMessage(), e);
                }
            }

            log.info("========================================");
            log.info("📊 [DailyReminderScheduler] 작업 완료 - 성공: {}, 실패: {}, 총: {}", 
                    successCount, failCount, users.size());
            log.info("========================================");
        } catch (Exception e) {
            log.error("❌ [DailyReminderScheduler] 일일 학습 알림 작업 중 예외 발생: {}", e.getMessage(), e);
            throw e; // 스케줄러 예외를 다시 던져서 Spring이 로그에 기록하도록 함
        }
    }
}








