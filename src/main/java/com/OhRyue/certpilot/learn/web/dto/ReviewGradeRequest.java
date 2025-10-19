package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record ReviewGradeRequest(
    Long userId,
    List<Item> answers
) {
  public record Item(Long questionId, Integer chosenIdx) {}
}
