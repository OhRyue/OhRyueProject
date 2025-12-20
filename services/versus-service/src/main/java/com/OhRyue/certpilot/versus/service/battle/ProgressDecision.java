package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

import java.util.Optional;

/**
 * 진행 결정
 * 
 * 다음 문제/라운드/종료를 결정합니다.
 */
@Value
@Builder
public class ProgressDecision {
    /**
     * 다음 stepKey (없으면 null)
     */
    Optional<String> nextStepKey;

    /**
     * 매치 종료 여부
     */
    boolean matchCompleted;

    /**
     * 라운드 완료 여부 (TOURNAMENT/GOLDENBELL에서 사용)
     */
    boolean roundCompleted;

    /**
     * 상태 변경 여부
     */
    boolean stateChanged;

    public static ProgressDecision none() {
        return ProgressDecision.builder()
                .nextStepKey(Optional.empty())
                .matchCompleted(false)
                .roundCompleted(false)
                .stateChanged(false)
                .build();
    }

    public static ProgressDecision nextQuestion(String nextStepKey) {
        return ProgressDecision.builder()
                .nextStepKey(Optional.of(nextStepKey))
                .matchCompleted(false)
                .roundCompleted(false)
                .stateChanged(true)
                .build();
    }

    public static ProgressDecision matchCompleted() {
        return ProgressDecision.builder()
                .nextStepKey(Optional.empty())
                .matchCompleted(true)
                .roundCompleted(false)
                .stateChanged(true)
                .build();
    }
}








