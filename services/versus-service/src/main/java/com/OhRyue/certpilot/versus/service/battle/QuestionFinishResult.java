package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

import java.util.Optional;

/**
 * 질문 종료 결과
 */
@Value
@Builder
public class QuestionFinishResult {
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
     * 다음 stepKey (없으면 null)
     */
    Optional<String> nextStepKey;

    /**
     * 매치 종료 여부
     */
    boolean matchCompleted;

    public static QuestionFinishResult skipped() {
        return QuestionFinishResult.builder()
                .processed(false)
                .skipped(true)
                .alreadyFinished(false)
                .nextStepKey(Optional.empty())
                .matchCompleted(false)
                .build();
    }

    public static QuestionFinishResult alreadyFinished() {
        return QuestionFinishResult.builder()
                .processed(false)
                .skipped(false)
                .alreadyFinished(true)
                .nextStepKey(Optional.empty())
                .matchCompleted(false)
                .build();
    }

    public static QuestionFinishResult completed(Optional<String> nextStepKey, boolean matchCompleted) {
        return QuestionFinishResult.builder()
                .processed(true)
                .skipped(false)
                .alreadyFinished(false)
                .nextStepKey(nextStepKey)
                .matchCompleted(matchCompleted)
                .build();
    }
}




