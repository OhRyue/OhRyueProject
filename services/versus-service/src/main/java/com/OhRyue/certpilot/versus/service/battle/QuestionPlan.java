package com.OhRyue.certpilot.versus.service.battle;

import lombok.Builder;
import lombok.Value;

import java.util.List;

/**
 * 질문 계획
 * 
 * 모드별 문제 구성을 정의합니다.
 */
@Value
@Builder
public class QuestionPlan {
    /**
     * 총 문제 수
     */
    int totalQuestions;

    /**
     * 총 라운드 수
     */
    int totalRounds;

    /**
     * 문제 목록 (라운드별, 순서별)
     */
    List<QuestionItem> questions;

    @Value
    @Builder
    public static class QuestionItem {
        /**
         * 라운드 번호
         */
        int roundNo;

        /**
         * 순서 번호
         */
        int orderNo;

        /**
         * 페이즈
         */
        String phase;

        /**
         * 문제 ID
         */
        Long questionId;

        /**
         * 제한시간 (초)
         */
        int timeLimitSec;
    }
}





