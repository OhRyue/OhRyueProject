package com.OhRyue.certpilot.progress.web.dto;

import java.time.Instant;
import java.util.List;

public record ProgressOverviewDto(
        Long userId,
        List<TagStat> tagStats,                 // 태그별 능력 추정 + 최근 오답
        List<RecentWrong> recentWrongs,        // 최신 오답 10개
        Instant generatedAt
) {
    public record TagStat(
            String tag,
            Double ema,          // 능력 EMA (0~1 가정) — 엔티티 필드명에 맞게 매핑
            Integer wrongCount   // 해당 태그의 누적/최근 오답 카운트(간이)
    ) {}

    public record RecentWrong(
            Long questionId,
            String tag,
            Instant lastWrongAt
    ) {}
}
