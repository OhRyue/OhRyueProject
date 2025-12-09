package com.OhRyue.certpilot.community.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(
    name = "account-service",
    path = "/api/account/internal",
    url = "${services.account.url:http://account-service:8081}",
    fallback = AccountServiceClientFallback.class
)
public interface AccountServiceClient {

    /**
     * 여러 사용자의 프로필 정보 조회 (닉네임, 스킨 ID 포함)
     */
    @GetMapping("/users/summaries")
    List<ProfileSummary> getUserProfiles(@RequestParam("ids") List<String> ids);

    record ProfileSummary(
        String userId,
        String nickname,
        Long skinId,
        String timezone,
        String lang
    ) {}
}





