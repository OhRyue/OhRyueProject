package com.OhRyue.certpilot.progress.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

import java.time.Instant;

/**
 * 배지 획득 이벤트
 * 배지가 지급될 때 발행됩니다.
 */
@Getter
public class BadgeEarnedEvent extends ApplicationEvent {
    
    private final String userId;
    private final String badgeCode;
    private final String badgeName;
    private final String badgeCategory;
    private final Instant earnedAt;
    
    public BadgeEarnedEvent(Object source, String userId, String badgeCode, 
                           String badgeName, String badgeCategory, Instant earnedAt) {
        super(source);
        this.userId = userId;
        this.badgeCode = badgeCode;
        this.badgeName = badgeName;
        this.badgeCategory = badgeCategory;
        this.earnedAt = earnedAt;
    }
}














