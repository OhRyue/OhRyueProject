package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.dto.ManualWeeklyReportRequest;
import com.OhRyue.certpilot.progress.dto.NotificationDtos;
import com.OhRyue.certpilot.progress.feign.AccountClient;
import com.OhRyue.certpilot.progress.repository.ReportDailyRepository;
import com.OhRyue.certpilot.progress.repository.ReportWeeklyRepository;
import com.OhRyue.certpilot.progress.repository.UserBadgeRepository;
import com.OhRyue.certpilot.progress.service.NotificationService;
import com.OhRyue.certpilot.progress.service.WeeklyReportService;
import com.OhRyue.certpilot.progress.service.WeeklyReportTemplateService;
import com.OhRyue.certpilot.progress.service.mail.MailSender;
import com.OhRyue.common.auth.AuthUserUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Notifications", description = "알림 APIs")
@RestController
@RequestMapping("/api/progress/notifications")
@RequiredArgsConstructor
@Slf4j
public class NotificationController {

    private final NotificationService notificationService;
    private final AccountClient accountClient;
    private final ReportWeeklyRepository reportWeeklyRepository;
    private final ReportDailyRepository reportDailyRepository;
    private final UserBadgeRepository userBadgeRepository;
    private final MailSender mailSender;
    private final WeeklyReportTemplateService weeklyReportTemplateService;
    private final WeeklyReportService weeklyReportService;

    @Operation(summary = "내 알림 목록 조회")
    @GetMapping("/my")
    public ResponseEntity<Page<NotificationDtos.NotificationResponse>> getMyNotifications(
            @RequestParam(required = false, defaultValue = "false") boolean unreadOnly,
            @RequestParam(required = false, defaultValue = "0") int page,
            @RequestParam(required = false, defaultValue = "20") int size) {
        String userId = AuthUserUtil.getCurrentUserId();
        Pageable pageable = PageRequest.of(page, size);
        Page<NotificationDtos.NotificationResponse> notifications = 
                notificationService.getMyNotifications(userId, unreadOnly, pageable);
        return ResponseEntity.ok(notifications);
    }

    @Operation(summary = "단일 알림 읽음 처리")
    @PostMapping("/{id}/read")
    public ResponseEntity<Void> markAsRead(@PathVariable Long id) {
        String userId = AuthUserUtil.getCurrentUserId();
        notificationService.markAsRead(userId, id);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "전체 알림 읽음 처리")
    @PostMapping("/read-all")
    public ResponseEntity<Void> markAllAsRead() {
        String userId = AuthUserUtil.getCurrentUserId();
        notificationService.markAllAsRead(userId);
        return ResponseEntity.ok().build();
    }

    @Operation(summary = "알림 생성 (내부 API, 다른 서비스에서 호출)")
    @PostMapping("/create")
    public ResponseEntity<Void> create(@RequestBody NotificationDtos.NotificationCreateRequest request) {
        try {
            log.info("Received notification create request: userId={}, type={}, title={}", 
                    request.userId(), request.type(), request.title());
            
            NotificationType type = NotificationType.valueOf(request.type());
            notificationService.createNotification(
                    request.userId(),
                    type,
                    request.title(),
                    request.message(),
                    request.payload()
            );
            
            log.info("Successfully created notification: userId={}, type={}", request.userId(), request.type());
            return ResponseEntity.ok().build();
        } catch (IllegalArgumentException e) {
            log.error("Invalid notification type: {}", request.type(), e);
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            log.error("Failed to create notification: userId={}, type={}, error={}", 
                    request.userId(), request.type(), e.getMessage(), e);
            return ResponseEntity.internalServerError().build();
        }
    }

    @Operation(summary = "일일 학습 알림 수동 발송 (테스트용)")
    @PostMapping("/test/daily-reminder")
    public ResponseEntity<NotificationDtos.TestResponse> testDailyReminder() {
        try {
            String currentUserId = AuthUserUtil.getCurrentUserId();
            log.info("Manual daily reminder trigger requested by user: {}", currentUserId);
            
            // 테스트용: 현재 사용자에게 직접 인앱 알림 발송 (커뮤니티 알림처럼)
            notificationService.createNotification(
                    currentUserId,
                    com.OhRyue.certpilot.progress.domain.enums.NotificationType.DAILY_REMINDER,
                    "오늘도 학습하실 준비 되셨나요?",
                    "하루 10문제라도 좋으니, 지금 바로 CertPilot에서 학습을 시작해 보세요!",
                    java.util.Map.of()
            );
            
            log.info("Daily reminder notification sent to user: {} (in-app notification)", currentUserId);
            return ResponseEntity.ok(new NotificationDtos.TestResponse(
                    true,
                    "일일 학습 알림이 발송되었습니다. 알림 목록에서 확인하세요."
            ));
        } catch (Exception e) {
            log.error("Failed to send daily reminder manually: {}", e.getMessage(), e);
            return ResponseEntity.ok(new NotificationDtos.TestResponse(
                    false,
                    "일일 학습 알림 발송 중 오류가 발생했습니다: " + e.getMessage()
            ));
        }
    }

    @Operation(summary = "주간 학습 리포트 조회 (출력만, 메일 발송 없음)")
    @GetMapping("/weekly-report")
    public ResponseEntity<NotificationDtos.WeeklyReportResponse> getWeeklyReport() {
        try {
            String currentUserId = AuthUserUtil.getCurrentUserId();
            log.info("Weekly report preview requested by user: {}", currentUserId);
            
            // 현재 사용자 정보 조회
            com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse me = accountClient.me();
            String email = me != null && me.account() != null && me.account().email() != null 
                    ? me.account().email() 
                    : "이메일 미등록";
            String nickname = me != null && me.profile() != null && me.profile().nickname() != null 
                    ? me.profile().nickname() 
                    : currentUserId;
            
            // 지난 주 ISO 주차 계산
            java.time.LocalDate today = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Seoul"));
            java.time.temporal.WeekFields weekFields = java.time.temporal.WeekFields.ISO;
            
            // 지난 주의 같은 요일 계산
            java.time.LocalDate lastWeekSameDay = today.minusWeeks(1);
            
            // 지난 주의 ISO 주차 계산
            int week = lastWeekSameDay.get(weekFields.weekOfWeekBasedYear());
            int year = lastWeekSameDay.get(weekFields.weekBasedYear());
            String weekIso = String.format("%d-W%02d", year, week);
            
            // 지난 주의 시작일(월요일)과 종료일(일요일) 계산
            java.time.LocalDate lastWeekStart = lastWeekSameDay.with(weekFields.dayOfWeek(), 1); // ISO 주의 시작일(월요일)
            java.time.LocalDate lastWeekEnd = lastWeekStart.plusDays(6); // 일요일
            
            // 지난 주 리포트 조회 (없으면 기본값 사용)
            com.OhRyue.certpilot.progress.domain.ReportWeekly report = reportWeeklyRepository
                    .findByUserIdAndWeekIso(currentUserId, weekIso)
                    .orElseGet(() -> {
                        log.debug("No weekly report found for user {} week {}, using default values", currentUserId, weekIso);
                        return com.OhRyue.certpilot.progress.domain.ReportWeekly.builder()
                                .userId(currentUserId)
                                .weekIso(weekIso)
                                .solvedCount(0)
                                .correctCount(0)
                                .timeSpentSec(0)
                                .build();
                    });
            
            // 지난 주 기간 내에 획득한 배지 조회 (지난 주 월요일 00:00 ~ 일요일 23:59:59)
            java.time.Instant lastWeekStartInstant = lastWeekStart.atStartOfDay(java.time.ZoneId.of("Asia/Seoul")).toInstant();
            java.time.Instant lastWeekEndInstant = lastWeekEnd.atTime(23, 59, 59).atZone(java.time.ZoneId.of("Asia/Seoul")).toInstant();
            
            java.util.List<com.OhRyue.certpilot.progress.domain.UserBadge> newBadges = 
                    userBadgeRepository.findByUserIdOrderByEarnedAtDesc(currentUserId).stream()
                            .filter(badge -> badge.getEarnedAt() != null 
                                    && !badge.getEarnedAt().isBefore(lastWeekStartInstant)
                                    && !badge.getEarnedAt().isAfter(lastWeekEndInstant))
                            .collect(java.util.stream.Collectors.toList());
            
            // 지난 주에 실제로 학습한 일수 계산 (solvedCount > 0인 날짜의 개수)
            java.util.List<com.OhRyue.certpilot.progress.domain.ReportDaily> lastWeekDailyReports = 
                    reportDailyRepository.findByUserIdAndDateBetween(currentUserId, lastWeekStart, lastWeekEnd);
            int studyDaysInLastWeek = (int) lastWeekDailyReports.stream()
                    .filter(daily -> daily.getSolvedCount() > 0)
                    .count();
            
            // 리포트에서는 지난 주에 학습한 일수 사용
            int streakDays = studyDaysInLastWeek;
            
            // 리포트 통계
            int totalSolved = report.getSolvedCount();
            int totalCorrect = report.getCorrectCount();
            double accuracy = totalSolved > 0 ? (double) totalCorrect / totalSolved * 100.0 : 0.0;
            int totalStudyMinutes = report.getTimeSpentSec() / 60;
            
            // 메일 본문 생성
            StringBuilder emailBody = new StringBuilder();
            emailBody.append("안녕하세요, ").append(nickname).append("님!\n\n");
            emailBody.append("CertPilot 주간 학습 리포트입니다.\n\n");
            emailBody.append("=== 지난 주 학습 통계 ===\n");
            emailBody.append("• 해결한 문제 수: ").append(totalSolved).append("문제\n");
            emailBody.append("• 평균 정답률: ").append(String.format("%.1f", accuracy)).append("%\n");
            emailBody.append("• 총 학습 시간: ").append(totalStudyMinutes).append("분\n");
            emailBody.append("• 새로 획득한 배지: ").append(newBadges.size()).append("개\n");
            emailBody.append("• 연속 학습 일수: ").append(streakDays).append("일\n\n");
            emailBody.append("이번 주도 화이팅하세요! 🚀\n\n");
            emailBody.append("CertPilot 팀 드림");
            
            // 메일 제목 생성
            String subject = String.format("[CertPilot] %s 주간 학습 리포트", weekIso);
            
            NotificationDtos.WeeklyReportResponse response = new NotificationDtos.WeeklyReportResponse(
                    weekIso,
                    nickname,
                    email,
                    totalSolved,
                    totalCorrect,
                    accuracy,
                    totalStudyMinutes,
                    newBadges.size(),
                    streakDays,
                    subject,
                    emailBody.toString()
            );
            
            log.info("Weekly report preview generated for user {}", currentUserId);
            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("Failed to generate weekly report preview: {}", e.getMessage(), e);
            throw new RuntimeException("주간 학습 리포트 조회 중 오류가 발생했습니다: " + e.getMessage(), e);
        }
    }

    @Operation(summary = "주간 학습 리포트 이메일 수동 발송 (테스트용)")
    @PostMapping("/test/weekly-report")
    public ResponseEntity<NotificationDtos.TestResponse> testWeeklyReport() {
        try {
            String currentUserId = AuthUserUtil.getCurrentUserId();
            log.info("Manual weekly report trigger requested by user: {}", currentUserId);
            
            // 현재 사용자 정보 조회
            com.OhRyue.certpilot.progress.feign.dto.AccountMeResponse me = accountClient.me();
            if (me == null || me.account() == null || me.account().email() == null || me.account().email().isBlank()) {
                return ResponseEntity.ok(new NotificationDtos.TestResponse(
                        false,
                        "이메일 주소가 등록되지 않았습니다. 계정 설정에서 이메일을 등록해주세요."
                ));
            }
            
            String email = me.account().email();
            String nickname = me.profile() != null && me.profile().nickname() != null 
                    ? me.profile().nickname() 
                    : currentUserId;
            
            // 지난 주 ISO 주차 계산
            java.time.LocalDate today = java.time.LocalDate.now(java.time.ZoneId.of("Asia/Seoul"));
            java.time.temporal.WeekFields weekFields = java.time.temporal.WeekFields.ISO;
            
            // 지난 주의 같은 요일 계산
            java.time.LocalDate lastWeekSameDay = today.minusWeeks(1);
            
            // 지난 주의 ISO 주차 계산
            int week = lastWeekSameDay.get(weekFields.weekOfWeekBasedYear());
            int year = lastWeekSameDay.get(weekFields.weekBasedYear());
            String weekIso = String.format("%d-W%02d", year, week);
            
            // 지난 주의 시작일(월요일)과 종료일(일요일) 계산
            java.time.LocalDate lastWeekStart = lastWeekSameDay.with(weekFields.dayOfWeek(), 1);
            java.time.LocalDate lastWeekEnd = lastWeekStart.plusDays(6);
            
            // 지난 주 리포트 조회 (없으면 기본값 사용)
            com.OhRyue.certpilot.progress.domain.ReportWeekly report = reportWeeklyRepository
                    .findByUserIdAndWeekIso(currentUserId, weekIso)
                    .orElseGet(() -> {
                        log.warn("No weekly report found for user {} week {}, using default values", currentUserId, weekIso);
                        return com.OhRyue.certpilot.progress.domain.ReportWeekly.builder()
                                .userId(currentUserId)
                                .weekIso(weekIso)
                                .solvedCount(0)
                                .correctCount(0)
                                .timeSpentSec(0)
                                .build();
                    });
            
            // 지난 주 기간 내에 획득한 배지 조회
            java.time.Instant lastWeekStartInstant = lastWeekStart.atStartOfDay(java.time.ZoneId.of("Asia/Seoul")).toInstant();
            java.time.Instant lastWeekEndInstant = lastWeekEnd.atTime(23, 59, 59).atZone(java.time.ZoneId.of("Asia/Seoul")).toInstant();
            
            java.util.List<com.OhRyue.certpilot.progress.domain.UserBadge> newBadges = 
                    userBadgeRepository.findByUserIdOrderByEarnedAtDesc(currentUserId).stream()
                            .filter(badge -> badge.getEarnedAt() != null 
                                    && !badge.getEarnedAt().isBefore(lastWeekStartInstant)
                                    && !badge.getEarnedAt().isAfter(lastWeekEndInstant))
                            .collect(java.util.stream.Collectors.toList());
            
            // 지난 주에 실제로 학습한 일수 계산
            java.util.List<com.OhRyue.certpilot.progress.domain.ReportDaily> lastWeekDailyReports = 
                    reportDailyRepository.findByUserIdAndDateBetween(currentUserId, lastWeekStart, lastWeekEnd);
            int studyDaysInLastWeek = (int) lastWeekDailyReports.stream()
                    .filter(daily -> daily.getSolvedCount() > 0)
                    .count();
            
            // 리포트 통계
            int totalSolved = report.getSolvedCount();
            int totalCorrect = report.getCorrectCount();
            double accuracy = totalSolved > 0 ? (double) totalCorrect / totalSolved * 100.0 : 0.0;
            int totalStudyMinutes = report.getTimeSpentSec() / 60;
            int streakDays = studyDaysInLastWeek;
            
            // 메일 발송 (카드형만 사용)
            String subject = String.format("[CertPilot] %s 주간 학습 리포트", weekIso);
            
            // 카드형 템플릿 사용
            String cardHtml = weeklyReportTemplateService.buildCardTemplate(
                    nickname, weekIso, totalSolved, totalCorrect, accuracy,
                    totalStudyMinutes, newBadges.size(), streakDays
            );
            mailSender.sendHtml(email, subject, cardHtml);
            
            log.info("Weekly report sent to user {} (email: {}), template: card", currentUserId, email);
            
            String message = String.format("주간 학습 리포트가 이메일(%s)로 발송되었습니다. 이메일을 확인하세요.", email);
            
            // 인앱 알림 기록
            notificationService.createNotification(
                    currentUserId,
                    NotificationType.WEEKLY_REPORT,
                    "주간 학습 리포트가 발송되었습니다",
                    String.format("지난 주 학습 리포트가 이메일(%s)로 발송되었습니다.", email),
                    java.util.Map.of(
                            "weekIso", weekIso,
                            "totalSolved", totalSolved,
                            "accuracy", accuracy,
                            "totalStudyMinutes", totalStudyMinutes,
                            "newBadgesCount", newBadges.size()
                    )
            );
            
            return ResponseEntity.ok(new NotificationDtos.TestResponse(true, message));
        } catch (Exception e) {
            log.error("Failed to send weekly report manually: {}", e.getMessage(), e);
            return ResponseEntity.ok(new NotificationDtos.TestResponse(
                    false,
                    "주간 학습 리포트 발송 중 오류가 발생했습니다: " + e.getMessage()
            ));
        }
    }

    @Operation(summary = "주간 학습 리포트 메일 수동 발송 (데모용, 하드코딩 데이터 사용)")
    @PostMapping("/weekly-report/manual-demo")
    public ResponseEntity<Void> sendManualWeeklyReport(@RequestBody ManualWeeklyReportRequest request) {
        weeklyReportService.sendManualDemoWeeklyReport(request.getEmail());
        return ResponseEntity.ok().build();
    }
}

