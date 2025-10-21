package com.OhRyue.certpilot.learn.web.dto;

import java.time.Instant;
import java.util.List;

public record RecentResultsDto(
    List<Micro> micros,
    List<Review> reviews
) {
  public record Micro(Long id, Long topicId, Long conceptId, int score, int total, Instant createdAt) {}
  public record Review(Long id, Long detailTopicId, int score, int total, Instant createdAt) {}
}
