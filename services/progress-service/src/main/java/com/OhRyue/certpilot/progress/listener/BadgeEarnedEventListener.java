package com.OhRyue.certpilot.progress.listener;

import com.OhRyue.certpilot.progress.domain.enums.NotificationType;
import com.OhRyue.certpilot.progress.event.BadgeEarnedEvent;
import com.OhRyue.certpilot.progress.service.NotificationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * 배지 획득 이벤트 리스너
 * 배지가 지급될 때 알림을 발송하거나 다른 처리를 수행합니다.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class BadgeEarnedEventListener {

  private final NotificationService notificationService;

  /**
   * 배지 획득 이벤트 처리
   * 비동기로 처리하여 메인 트랜잭션에 영향을 주지 않습니다.
   */
  @Async
  @EventListener
  public void handleBadgeEarned(BadgeEarnedEvent event) {
    try {
      log.info("Badge earned: userId={}, badgeCode={}, badgeName={}, category={}, earnedAt={}",
          event.getUserId(),
          event.getBadgeCode(),
          event.getBadgeName(),
          event.getBadgeCategory(),
          event.getEarnedAt());

      // 배지 획득 알림 생성
      notificationService.createNotification(
          event.getUserId(),
          NotificationType.BADGE_EARNED,
          "새 배지를 획득했어요!",
          String.format("'%s' 배지를 획득했습니다. 배지 목록에서 확인해보세요.", event.getBadgeName()),
          Map.of(
              "badgeCode", event.getBadgeCode(),
              "badgeName", event.getBadgeName(),
              "badgeCategory", event.getBadgeCategory() != null ? event.getBadgeCategory() : "UNKNOWN"
          )
      );

    } catch (Exception e) {
      log.error("Failed to handle badge earned event for user {} badge {}: {}",
          event.getUserId(), event.getBadgeCode(), e.getMessage(), e);
    }
  }
}



