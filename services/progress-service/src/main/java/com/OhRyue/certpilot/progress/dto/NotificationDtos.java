package com.OhRyue.certpilot.progress.dto;

import com.OhRyue.certpilot.progress.domain.enums.NotificationType;

import java.time.Instant;
import java.util.Map;

public class NotificationDtos {

    public record NotificationResponse(
            Long id,
            NotificationType type,
            String title,
            String message,
            Map<String, Object> payload,
            Boolean isRead,
            Instant createdAt,
            Instant readAt
    ) {}

    public record NotificationCreateRequest(
            String userId,
            String type,
            String title,
            String message,
            Map<String, Object> payload
    ) {}

    public record TestResponse(
            boolean success,
            String message
    ) {}

    public record WeeklyReportResponse(
            String weekIso,
            String nickname,
            String email,
            int totalSolved,
            int totalCorrect,
            double accuracy,
            int totalStudyMinutes,
            int newBadgesCount,
            int streakDays,
            String emailSubject,
            String emailBody
    ) {}
}

