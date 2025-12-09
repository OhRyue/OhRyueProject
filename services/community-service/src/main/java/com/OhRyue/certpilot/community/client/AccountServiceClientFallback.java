package com.OhRyue.certpilot.community.client;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * AccountServiceClient Fallback
 * - account-service 호출 실패 시 사용되는 Fallback 구현
 * - 닉네임 조회 실패 시 userId를 그대로 반환
 */
@Slf4j
@Component
public class AccountServiceClientFallback implements AccountServiceClient {

    @Override
    public List<ProfileSummary> getUserProfiles(List<String> ids) {
        log.warn("AccountServiceClient.getUserProfiles fallback 호출 - ids={}. 닉네임 조회 실패로 userId를 그대로 사용합니다.", ids);
        // Fallback: userId를 닉네임으로 사용
        return ids.stream()
                .map(userId -> new ProfileSummary(userId, userId, null, null, null))
                .toList();
    }
}


