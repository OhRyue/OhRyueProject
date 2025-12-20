package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

/**
 * 질문 단계 키 (stepKey) 파싱 결과
 * 
 * 모드별로 stepKey 형식이 다르므로, 파싱된 결과를 이 객체로 표현합니다.
 */
@Value
@Builder
public class StepKey {
    /**
     * 문제 ID (DUEL에서 사용)
     */
    Long questionId;

    /**
     * 라운드 번호 (TOURNAMENT/GOLDENBELL에서 사용)
     */
    Integer roundNo;

    /**
     * 순서 번호 (TOURNAMENT/GOLDENBELL에서 사용)
     */
    Integer orderNo;

    /**
     * 페이즈 (GOLDENBELL에서 사용)
     */
    String phase;

    /**
     * 원본 stepKey 문자열
     */
    String original;
}








