package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.domain.UserNotification;
import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.dto.NotificationDtos;
import com.OhRyue.certpilot.progress.repository.UserNotificationRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class NotificationService {

    private final UserNotificationRepository notificationRepository;
    private final ObjectMapper objectMapper;

    @Transactional
    public void createNotification(String userId, NotificationType type, String title, String message, Map<String, Object> payload) {
        try {
            String payloadJson = null;
            if (payload != null && !payload.isEmpty()) {
                payloadJson = objectMapper.writeValueAsString(payload);
            }

            UserNotification notification = UserNotification.builder()
                    .userId(userId)
                    .type(type)
                    .title(title)
                    .message(message)
                    .payloadJson(payloadJson)
                    .isRead(false)
                    .createdAt(Instant.now())
                    .build();

            notificationRepository.save(notification);
            log.info("Notification created: userId={}, type={}, title={}", userId, type, title);
        } catch (JsonProcessingException e) {
            log.error("Failed to serialize payload for notification: userId={}, type={}", userId, type, e);
            // payload 직렬화 실패해도 알림은 생성
            UserNotification notification = UserNotification.builder()
                    .userId(userId)
                    .type(type)
                    .title(title)
                    .message(message)
                    .payloadJson(null)
                    .isRead(false)
                    .createdAt(Instant.now())
                    .build();
            notificationRepository.save(notification);
        } catch (Exception e) {
            log.error("Failed to create notification: userId={}, type={}", userId, type, e);
            throw e;
        }
    }

    @Transactional(readOnly = true)
    public Page<NotificationDtos.NotificationResponse> getMyNotifications(String userId, boolean unreadOnly, Pageable pageable) {
        Page<UserNotification> notifications;
        if (unreadOnly) {
            notifications = notificationRepository.findByUserIdAndIsReadFalseOrderByCreatedAtDesc(userId, pageable);
        } else {
            notifications = notificationRepository.findByUserIdOrderByCreatedAtDesc(userId, pageable);
        }

        return notifications.map(this::toResponse);
    }

    @Transactional
    public void markAsRead(String userId, Long notificationId) {
        UserNotification notification = notificationRepository.findById(notificationId)
                .orElseThrow(() -> new IllegalArgumentException("Notification not found: " + notificationId));

        if (!notification.getUserId().equals(userId)) {
            throw new IllegalArgumentException("User does not have permission to read this notification");
        }

        if (!notification.getIsRead()) {
            notification.setIsRead(true);
            notification.setReadAt(Instant.now());
            notificationRepository.save(notification);
        }
    }

    @Transactional
    public void markAllAsRead(String userId) {
        Instant readAt = Instant.now();
        int updated = notificationRepository.markAllAsReadByUserId(userId, readAt);
        log.info("Marked {} notifications as read for user {}", updated, userId);
    }

    private NotificationDtos.NotificationResponse toResponse(UserNotification notification) {
        Map<String, Object> payload = null;
        if (notification.getPayloadJson() != null) {
            try {
                payload = objectMapper.readValue(notification.getPayloadJson(), Map.class);
            } catch (JsonProcessingException e) {
                log.warn("Failed to deserialize payload for notification {}: {}", notification.getId(), e.getMessage());
            }
        }

        return new NotificationDtos.NotificationResponse(
                notification.getId(),
                notification.getType(),
                notification.getTitle(),
                notification.getMessage(),
                payload,
                notification.getIsRead(),
                notification.getCreatedAt(),
                notification.getReadAt()
        );
    }
}













