package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnReviewSubmitRequest(
    Long userId,
    Long certId,
    Long detailTopicId,   // 세부항목 review topicId(레벨3)
    List<Item> answers
) {
  public record Item(Long questionId, Integer chosenIdx) {}
}
