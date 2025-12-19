package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

/**
 * 점수 계산 결과
 */
@Value
@Builder
public class ScoreResult {
    /**
     * 정답 여부
     */
    boolean correct;

    /**
     * 점수 변화량
     */
    int scoreDelta;

    /**
     * 제출 시간 (밀리초)
     */
    int timeMs;
}





