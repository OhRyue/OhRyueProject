package com.OhRyue.certpilot.learn.web.dto;

import java.time.Instant;
import java.util.List;

/** Micro 결과 상세 */
public record MicroResultDetailDto(
    Header header,
    List<Item> items
) {
  public record Header(Long id, Long userId, Long certId, Long topicId, Long conceptId,
                       int score, int total, Instant createdAt) {}
  public record Item(
      String itemType,   // "MINI" | "QUIZ"
      Long refId,        // concept_check.id or question.id
      String stem,
      List<String> choices,
      Integer chosenIdx,
      Boolean correct,
      String explanation,
      Integer ordNo
  ) {}
}
