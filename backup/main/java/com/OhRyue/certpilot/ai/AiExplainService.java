package com.OhRyue.certpilot.ai;

import com.OhRyue.certpilot.question.domain.Question;

import java.util.List;
import java.util.Map;

/** AI 해설 인터페이스 (실패 시 구현체에서 DB 해설 폴백) */
public interface AiExplainService {

    /**
     * 단일 문항 해설: 사용자가 고른 선택지(chosen)가 정답과 어떻게 다른지 설명.
     * @param q       문제
     * @param chosen  사용자가 고른 보기 index (null 허용)
     * @param dbExp   DB 기본 해설 (폴백용)
     */
    String explainForQuestion(Question q, Integer chosen, String dbExp);

    /**
     * 리뷰(20문항) 요약: 정오표 + 약점 태그 목록을 바탕으로 요약 메시지 생성.
     * @param correctness  문제ID -> 정답 여부
     * @param weakTags     약점 태그(간이)
     */
    String summarizeReview(Map<Long, Boolean> correctness, List<String> weakTags);
}
