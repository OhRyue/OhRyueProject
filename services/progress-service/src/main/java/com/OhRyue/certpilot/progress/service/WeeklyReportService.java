package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.ReportDaily;
import com.OhRyue.certpilot.progress.domain.ReportWeekly;
import com.OhRyue.certpilot.progress.domain.UserBadge;
import com.OhRyue.certpilot.progress.domain.UserNotification;
import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;
import com.OhRyue.certpilot.progress.repository.UserBadgeRepository;
import com.OhRyue.certpilot.progress.service.mail.MailSender;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.WeekFields;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 주간 학습 리포트 서비스
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WeeklyReportService {

    private static final ZoneId KST = ZoneId.of("Asia/Seoul");

    private final ReportWeeklyRepository reportWeeklyRepository;
    private final ReportDailyRepository reportDailyRepository;
    private final UserBadgeRepository userBadgeRepository;
    private final NotificationService notificationService;
    private final MailSender mailSender;
    private final AccountClient accountClient;
    private final WeeklyReportTemplateService weeklyReportTemplateService;

    /**
     * TODO: 데모용 manual weekly report, 테스트 끝나면 제거
     */
    public void sendManualDemoWeeklyReport(String email) {
        if (email == null || email.isBlank()) {
            log.warn("Manual demo weekly report requested with empty email, skipping");
            return;
        }

        // 고정 데이터
        String weekIso = "2025-W49";
        String nickname = "김치보끔밥";
        int totalSolved = 124;
        int totalCorrect = 93;
        double accuracy = 75.0;
        int totalStudyMinutes = 210;
        int newBadgesCount = 2;
        int streakDays = 4;

        String subject = String.format("[CertPilot] %s 주간 학습 리포트", weekIso);

        // 카드형 템플릿 사용 (기존 템플릿 재활용)
        String cardHtml = weeklyReportTemplateService.buildCardTemplate(
                nickname,
                weekIso,
                totalSolved,
                totalCorrect,
                accuracy,
                totalStudyMinutes,
                newBadgesCount,
                streakDays
        );

        mailSender.sendHtml(email, subject, cardHtml);
        log.info("Manual demo weekly report sent to {}", email);
    }

    @Transactional(readOnly = true)
    public void sendWeeklyReportsForAllUsers() {
        try {
            log.info("Starting weekly report job");
            
            // 주간 리포트를 받을 사용자 목록 조회
            List<AccountClient.UserWithEmail> users = accountClient.getUsersWithWeeklyReportEnabled();
            log.info("Found {} users with weekly report enabled", users.size());

            // 지난 주 ISO 주차 계산
            LocalDate today = LocalDate.now(KST);
            LocalDate lastWeek = today.minusWeeks(1);
            WeekFields weekFields = WeekFields.ISO;
            int week = lastWeek.get(weekFields.weekOfWeekBasedYear());
            int year = lastWeek.get(weekFields.weekBasedYear());
            String weekIso = String.format("%d-W%02d", year, week);

            log.info("Processing weekly report for week: {}", weekIso);

            for (AccountClient.UserWithEmail user : users) {
                try {
                    if (user.email() == null || user.email().isBlank()) {
                        log.warn("User {} has no email, skipping weekly report", user.userId());
                        continue;
                    }

                    // 지난 주 리포트 조회
                    ReportWeekly report = reportWeeklyRepository.findByUserIdAndWeekIso(user.userId(), weekIso)
                            .orElse(null);
                    
                    if (report == null) {
                        log.debug("No weekly report found for user {} week {}", user.userId(), weekIso);
                        continue;
                    }

                    sendWeeklyReportForUser(user.userId(), user.email(), report, weekIso);
                } catch (Exception e) {
                    log.error("Failed to send weekly report for user {}: {}", user.userId(), e.getMessage(), e);
                }
            }

            log.info("Completed weekly report job for {} users", users.size());
        } catch (Exception e) {
            log.error("Failed to send weekly reports: {}", e.getMessage(), e);
        }
    }

    private void sendWeeklyReportForUser(String userId, String email, ReportWeekly report, String weekIso) {
        try {
            // 지난 주의 시작일(월요일)과 종료일(일요일) 계산
            LocalDate today = LocalDate.now(KST);
            WeekFields weekFields = WeekFields.ISO;
            LocalDate lastWeekSameDay = today.minusWeeks(1);
            LocalDate lastWeekStart = lastWeekSameDay.with(weekFields.dayOfWeek(), 1); // ISO 주의 시작일(월요일)
            LocalDate lastWeekEnd = lastWeekStart.plusDays(6); // 일요일

            // 지난 주 기간 내에 획득한 배지 조회 (지난 주 월요일 00:00 ~ 일요일 23:59:59)
            Instant lastWeekStartInstant = lastWeekStart.atStartOfDay(KST).toInstant();
            Instant lastWeekEndInstant = lastWeekEnd.atTime(23, 59, 59).atZone(KST).toInstant();
            
            List<UserBadge> newBadges = userBadgeRepository.findByUserIdOrderByEarnedAtDesc(userId).stream()
                    .filter(badge -> badge.getEarnedAt() != null
                            && !badge.getEarnedAt().isBefore(lastWeekStartInstant)
                            && !badge.getEarnedAt().isAfter(lastWeekEndInstant))
                    .collect(Collectors.toList());

            // 지난 주에 실제로 학습한 일수 계산 (solvedCount > 0인 날짜의 개수)
            List<ReportDaily> lastWeekDailyReports = 
                    reportDailyRepository.findByUserIdAndDateBetween(userId, lastWeekStart, lastWeekEnd);
            int studyDaysInLastWeek = (int) lastWeekDailyReports.stream()
                    .filter(daily -> daily.getSolvedCount() > 0)
                    .count();

            // 리포트 통계
            int totalSolved = report.getSolvedCount();
            int totalCorrect = report.getCorrectCount();
            double accuracy = totalSolved > 0 ? (double) totalCorrect / totalSolved * 100.0 : 0.0;
            int totalStudyMinutes = report.getTimeSpentSec() / 60;
            int streakDays = studyDaysInLastWeek;

            // 프로필 정보 조회 (닉네임)
            String nickname = userId; // 기본값은 userId
            try {
                List<com.OhRyue.certpilot.progress.feign.dto.ProfileSummaryResponse> profiles = 
                        accountClient.summaries(java.util.List.of(userId));
                if (!profiles.isEmpty() && profiles.get(0).nickname() != null) {
                    nickname = profiles.get(0).nickname();
                }
            } catch (Exception e) {
                log.warn("Failed to fetch profile for user {}: {}", userId, e.getMessage());
            }

            // 카드형 HTML 템플릿 생성
            String emailBody = weeklyReportTemplateService.buildCardTemplate(
                    nickname, weekIso, totalSolved, totalCorrect, accuracy,
                    totalStudyMinutes, newBadges.size(), streakDays
            );

            // 메일 발송 (HTML)
            String subject = String.format("[CertPilot] %s 주간 학습 리포트", weekIso);
            try {
                mailSender.sendHtml(email, subject, emailBody);
                log.info("Weekly report email sent successfully to user {} (email: {})", userId, email);
            } catch (Exception e) {
                log.error("Failed to send weekly report email to user {} (email: {}): {}", 
                        userId, email, e.getMessage(), e);
                throw e; // 메일 발송 실패 시 예외를 다시 던져서 알림 생성하지 않음
            }

            // 메일 발송 성공 후 인앱 알림 기록 (멱등성 체크 포함)
            try {
                // 멱등성 체크: 같은 주차에 대한 알림이 이미 존재하는지 확인
                String weekIsoPattern = "%\"weekIso\":\"" + weekIso + "\"%";
                List<UserNotification> existingNotifications = 
                        notificationService.findByUserIdAndTypeAndWeekIso(userId, NotificationType.WEEKLY_REPORT, weekIsoPattern);
                
                if (!existingNotifications.isEmpty()) {
                    log.info("Weekly report notification already exists for user {} week {}, skipping notification creation", 
                            userId, weekIso);
                } else {
                    notificationService.createNotification(
                            userId,
                            NotificationType.WEEKLY_REPORT,
                            "주간 학습 리포트 발송",
                            "주간 학습 리포트가 발송되었습니다. 메일을 확인해주세요.",
                            Map.of(
                                    "weekIso", weekIso,
                                    "totalSolved", totalSolved,
                                    "accuracy", accuracy,
                                    "totalStudyMinutes", totalStudyMinutes,
                                    "newBadgesCount", newBadges.size()
                            )
                    );
                    log.info("Weekly report notification created for user {} (week: {})", userId, weekIso);
                }
            } catch (Exception e) {
                // 알림 생성 실패는 로그만 남기고 메일 발송 성공은 유지
                log.error("Failed to create weekly report notification for user {} (week: {}): {}", 
                        userId, weekIso, e.getMessage(), e);
            }

            log.info("Weekly report sent to user {} (email: {})", userId, email);
        } catch (Exception e) {
            log.error("Failed to send weekly report for user {}: {}", userId, e.getMessage(), e);
            throw e;
        }
    }
}

