package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnReviewSubmitRequest(
    Long userId,
    List<Answer> answers
) {
  public record Answer(Long id, Integer choiceIdx) {}
}
