package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnReviewStartDto(
    Long detailTopicId,
    int total,
    List<Quiz> quiz //20문항
) {
  public record Quiz(Long id, String stem, List<String> choices, Integer difficulty) {}
}
