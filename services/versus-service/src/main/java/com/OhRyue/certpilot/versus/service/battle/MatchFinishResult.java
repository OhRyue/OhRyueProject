package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

/**
 * 매치 종료 결과
 */
@Value
@Builder
public class MatchFinishResult {
    /**
     * 처리 성공 여부
     */
    boolean processed;

    /**
     * 스킵 여부 (락 획득 실패)
     */
    boolean skipped;

    /**
     * 이미 종료됨 여부
     */
    boolean alreadyFinished;

    /**
     * 승자
     */
    String winner;

    /**
     * XP 지급 여부
     */
    boolean xpGranted;

    public static MatchFinishResult skipped() {
        return MatchFinishResult.builder()
                .processed(false)
                .skipped(true)
                .alreadyFinished(false)
                .winner(null)
                .xpGranted(false)
                .build();
    }

    public static MatchFinishResult alreadyFinished() {
        return MatchFinishResult.builder()
                .processed(false)
                .skipped(false)
                .alreadyFinished(true)
                .winner(null)
                .xpGranted(false)
                .build();
    }

    public static MatchFinishResult completed(String winner, boolean xpGranted) {
        return MatchFinishResult.builder()
                .processed(true)
                .skipped(false)
                .alreadyFinished(false)
                .winner(winner)
                .xpGranted(xpGranted)
                .build();
    }
}






