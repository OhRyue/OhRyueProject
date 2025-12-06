package com.OhRyue.certpilot.study.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
    name = "progress-xp-client",
    url = "${GATEWAY_BASE_URL:http://gateway:8080}/api/progress/xp"
)
public interface ProgressXpClient {

    @PostMapping("/earn")
    XpEarnResponse earnXp(@RequestBody XpEarnRequest req);

    @PostMapping("/gain")
    void gainXp(@RequestParam("delta") int delta,
                @RequestParam(value = "reason", defaultValue = "ASSIST") String reason,
                @RequestParam(value = "refId", required = false) String refId);

    @GetMapping("/wallet")
    XpWalletResponse getWallet();

    record XpEarnRequest(
            String activityType,  // WRITTEN_MICRO, PRACTICAL_MICRO, WRITTEN_REVIEW, PRACTICAL_REVIEW 등
            Long sessionId,        // study_session ID (중복 XP 지급 방지용)
            Long topicId,          // 선택 (진척도 계산할 때 필요할 수 있음)
            Double scorePct        // 선택 (정답률 기반 XP 계산용, 0.0 ~ 100.0)
    ) {}

    record XpEarnResponse(
            int earnedXp,          // 이번에 획득한 XP
            long totalXp,          // 지급 후 총 XP
            int level,             // 현재 레벨
            int xpToNextLevel,     // 다음 레벨까지 필요한 XP
            boolean leveledUp,     // 이번 지급으로 인해 레벨업 O/X
            int levelUpRewardPoints // 레벨업 보상 포인트 (없으면 0)
    ) {}

    record XpWalletResponse(
            long xpTotal,          // 현재 총 XP
            int level,             // 현재 레벨
            int xpToNextLevel      // 다음 레벨까지 필요한 XP (계산 필요)
    ) {}
}

