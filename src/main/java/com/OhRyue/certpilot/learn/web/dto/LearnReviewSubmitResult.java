package com.OhRyue.certpilot.learn.web.dto;

import java.util.Map;

/**
 * Review(세부항목 20문제) 채점 결과
 * - correctness: 문제ID -> 정답 여부
 * - explanations: 문제ID -> AI 해설(실패 시 DB 해설 폴백)
 * - aiSummary: 전체 요약(반복 실수/추가 학습 포인트)
 */
public record LearnReviewSubmitResult(
        int score,
        int total,
        Map<Long, Boolean> correctness,
        Map<Long, String> explanations,
        String aiSummary
) {}
