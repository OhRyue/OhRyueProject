package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

/** Review(세부항목 20문제) 채점 요청 */
public record LearnReviewSubmitRequest(
        Long userId,
        Long detailTopicId,                // 세부항목 topicId (저장용)
        List<LearnReviewSubmitRequest.Item> answers
) {
    public record Item(Long questionId, Integer chosenIdx) {}
}
