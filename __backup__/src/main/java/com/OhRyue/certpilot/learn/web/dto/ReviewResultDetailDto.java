package com.OhRyue.certpilot.learn.web.dto;

import java.time.Instant;
import java.util.List;

/** Review 결과 상세 */
public record ReviewResultDetailDto(
    Header header,
    List<Item> items
) {
  public record Header(Long id, Long userId, Long certId, Long detailTopicId,
                       int score, int total, String aiSummary, Instant createdAt) {}
  public record Item(
      Long questionId,
      String stem,
      List<String> choices,
      Integer chosenIdx,
      Boolean correct,
      String aiExplanation,
      Integer ordNo
  ) {}
}
