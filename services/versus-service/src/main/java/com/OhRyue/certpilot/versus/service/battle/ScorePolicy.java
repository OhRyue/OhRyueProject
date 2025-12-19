package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

/**
 * 점수 정책
 * 
 * 모드별 점수 계산 규칙을 정의합니다.
 */
@Value
@Builder
public class ScorePolicy {
    /**
     * 속도 보너스 활성화 여부
     */
    boolean speedBonusEnabled;

    /**
     * 오답 패널티
     */
    int wrongAnswerPenalty;

    /**
     * 타임아웃 패널티
     */
    int timeoutPenalty;
}





