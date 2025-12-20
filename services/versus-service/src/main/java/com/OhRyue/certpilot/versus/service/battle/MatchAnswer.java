package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

/**
 * 답안 정보 (Strategy에서 사용)
 * 
 * 실제 MatchAnswer 엔티티와는 별도로, Strategy에서 사용하기 위한 간단한 DTO입니다.
 */
@Value
@Builder
public class MatchAnswer {
    /**
     * 사용자 ID
     */
    String userId;

    /**
     * 문제 ID
     */
    Long questionId;

    /**
     * 사용자 답안
     */
    String userAnswer;

    /**
     * 정답 여부
     */
    boolean correct;

    /**
     * 제출 시간 (밀리초)
     */
    int timeMs;

    /**
     * 점수 변화량
     */
    int scoreDelta;
}








