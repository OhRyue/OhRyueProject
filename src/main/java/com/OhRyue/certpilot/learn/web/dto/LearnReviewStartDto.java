package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnReviewStartDto(
    Long detailTopicId,
    String detailTopicName,
    List<Quiz> quiz20 // 20문항
) {
  public record Quiz(Long id, String stem, List<String> choices, Integer difficulty) {}
}
