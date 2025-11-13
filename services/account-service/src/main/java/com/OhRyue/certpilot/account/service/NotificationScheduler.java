package com.OhRyue.certpilot.account.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.temporal.WeekFields;
import java.util.Locale;

@Component
@RequiredArgsConstructor
@Slf4j
public class NotificationScheduler {

  private final NotificationQueueService queueService;
  private final NotificationSender sender;

  /**
   * 매일 오전 7시(서울) 기준으로 하루 목표 알림 큐잉.
   */
  @Scheduled(cron = "0 0 7 * * *", zone = "Asia/Seoul")
  public void scheduleDailyGoals() {
    LocalDate today = LocalDate.now();
    queueService.enqueueDailyGoals(today);
    log.debug("Enqueued daily goal notifications for {}", today);
  }

  /**
   * 매주 월요일 오전 6시에 주간 요약 알림 큐잉.
   */
  @Scheduled(cron = "0 0 6 * * MON", zone = "Asia/Seoul")
  public void scheduleWeeklySummary() {
    LocalDate monday = LocalDate.now().with(WeekFields.of(Locale.KOREA).dayOfWeek(), 1);
    queueService.enqueueWeeklySummary(monday);
    log.debug("Enqueued weekly summary notifications for week {}", monday);
  }

  /**
   * 5분마다 큐에 쌓인 알림을 발송.
   */
  @Scheduled(fixedDelayString = "PT5M")
  public void dispatch() {
    queueService.dispatchPending(sender);
  }
}

