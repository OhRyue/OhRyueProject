package com.OhRyue.certpilot.progress.scheduler;

import com.OhRyue.certpilot.progress.service.WeeklyReportService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * 주간 학습 리포트 스케줄러
 * 매주 월요일 09:00 (Asia/Seoul)에 주간 리포트를 이메일로 발송합니다.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class WeeklyReportScheduler {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final WeeklyReportService weeklyReportService;

    @Scheduled(cron = "0 0 9 * * MON", zone = "Asia/Seoul")
    public void sendWeeklyReports() {
        LocalDateTime now = LocalDateTime.now(KST);
        java.time.ZoneId systemDefault = java.time.ZoneId.systemDefault();
        
        log.info("========================================");
        log.info("📅 [WeeklyReportScheduler] 스케줄러 실행 시작");
        log.info("   현재 시간 (Asia/Seoul): {}", now.format(FORMATTER));
        log.info("   시스템 기본 타임존: {}", systemDefault);
        log.info("   JVM 타임존: {}", System.getProperty("user.timezone", "미설정"));
        log.info("   환경변수 TZ: {}", System.getenv("TZ"));
        log.info("========================================");
        
        try {
            weeklyReportService.sendWeeklyReportsForAllUsers();
            log.info("========================================");
            log.info("✅ [WeeklyReportScheduler] 주간 학습 리포트 발송 작업 완료");
            log.info("========================================");
        } catch (Exception e) {
            log.error("❌ [WeeklyReportScheduler] 주간 학습 리포트 발송 작업 중 예외 발생: {}", e.getMessage(), e);
            throw e; // 스케줄러 예외를 다시 던져서 Spring이 로그에 기록하도록 함
        }
    }
}








