package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

import java.util.List;

/**
 * 탈락 결정
 * 
 * TOURNAMENT/GOLDENBELL에서 사용합니다.
 */
@Value
@Builder
public class EliminationDecision {
    /**
     * 탈락 대상 사용자 ID 목록
     */
    List<String> eliminatedUserIds;

    /**
     * 탈락 사유
     */
    String reason;

    /**
     * 탈락 활성화 여부
     */
    boolean enabled;

    public static EliminationDecision none() {
        return EliminationDecision.builder()
                .eliminatedUserIds(List.of())
                .reason("NONE")
                .enabled(false)
                .build();
    }

    public static EliminationDecision eliminate(List<String> userIds, String reason) {
        return EliminationDecision.builder()
                .eliminatedUserIds(userIds)
                .reason(reason)
                .enabled(true)
                .build();
    }
}







