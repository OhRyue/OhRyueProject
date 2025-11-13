package com.OhRyue.certpilot.account.domain;

import com.OhRyue.certpilot.account.domain.enums.NotificationChannel;
import com.OhRyue.certpilot.account.domain.enums.NotificationStatus;
import com.OhRyue.certpilot.account.domain.enums.NotificationType;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "notification_job",
    indexes = {
        @Index(name = "ix_notification_status", columnList = "status, scheduled_at")
    })
@Getter
@Setter
public class NotificationJob {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "user_id", nullable = false, length = 100)
  private String userId;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private NotificationChannel channel;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 20)
  private NotificationType type;

  @Column(name = "payload_json", columnDefinition = "MEDIUMTEXT")
  private String payloadJson;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private NotificationStatus status = NotificationStatus.PENDING;

  @Column(name = "scheduled_at", nullable = false)
  private Instant scheduledAt;

  @Column(name = "sent_at")
  private Instant sentAt;

  @Column(name = "retry_count", nullable = false)
  private int retryCount;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @Column(name = "updated_at")
  private Instant updatedAt;

  @PrePersist
  void onCreate() {
    createdAt = Instant.now();
    updatedAt = createdAt;
    if (status == null) {
      status = NotificationStatus.PENDING;
    }
  }

  @PreUpdate
  void onUpdate() {
    updatedAt = Instant.now();
  }
}

