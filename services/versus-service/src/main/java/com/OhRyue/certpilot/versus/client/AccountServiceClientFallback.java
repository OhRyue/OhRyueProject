package com.OhRyue.certpilot.versus.client;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
public class AccountServiceClientFallback implements AccountServiceClient {

    @Override
    public List<ProfileSummary> getUserProfiles(List<String> ids) {
        log.warn("AccountServiceClient fallback: 사용자 프로필 조회 실패. userId 목록: {}", ids);
        // Fallback: 기본값 반환
        // - 봇인 경우: userId 기반 고정 스킨 ID (1~17)
        // - 일반 사용자인 경우: 기본 스킨 ID 1
        return ids.stream()
                .map(userId -> {
                    boolean isBot = userId.startsWith("BOT_");
                    Long skinId = isBot ? getFixedSkinIdForBot(userId) : 1L;
                    return new ProfileSummary(userId, null, skinId, null, null);
                })
                .collect(Collectors.toList());
    }

    /**
     * 봇의 userId를 기반으로 고정된 스킨 ID를 반환 (1~17)
     */
    private Long getFixedSkinIdForBot(String userId) {
        int hash = userId.hashCode();
        // 음수 방지 및 1~17 범위로 매핑
        return (long) (Math.abs(hash) % 17 + 1);
    }
}

