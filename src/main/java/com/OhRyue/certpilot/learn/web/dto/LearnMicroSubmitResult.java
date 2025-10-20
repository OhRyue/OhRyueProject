package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;
import java.util.Map;

/**
 * Micro(세세항목) 제출 결과
 * - score/total: 미니체크+퀴즈 합산 점수/문항수
 * - wrongIds: 오답 ID 목록(미니/퀴즈 혼재 — 필요 시 프런트에서 구분 가능)
 * - miniExplanations: concept_check 해설(DB explanation)
 * - quizExplanations: question 해설(AI → 폴백 DB)
 */
public record LearnMicroSubmitResult(
        int score,                       // 총점(미니+퀴즈 합산)
        int total,                       // 총 문항수
        List<Long> wrongIds,             // 오답 ID 목록(미니/퀴즈 혼재)
        Map<Long, String> miniExplanations, // concept_check 해설(DB)
        Map<Long, String> quizExplanations  // question 해설(AI → 폴백 DB)
) {}
