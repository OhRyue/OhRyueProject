package com.OhRyue.certpilot.progress.scheduler;

import com.OhRyue.certpilot.progress.service.WeeklyReportService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * 주간 학습 리포트 스케줄러
 * 매주 월요일 09:00 (Asia/Seoul)에 주간 리포트를 이메일로 발송합니다.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class WeeklyReportScheduler {

    private final WeeklyReportService weeklyReportService;

    @Scheduled(cron = "0 0 9 * * MON", zone = "Asia/Seoul")
    public void sendWeeklyReports() {
        log.info("Starting weekly report scheduler");
        weeklyReportService.sendWeeklyReportsForAllUsers();
        log.info("Completed weekly report scheduler");
    }
}





