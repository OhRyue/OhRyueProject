package com.OhRyue.certpilot.shared.dto;

import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LearnSessionDto {
  private String sessionId;
  private LocalDateTime startedAt;
  private List<QuestionDto> questions;

  public static LearnSessionDto ofNow(List<QuestionDto> qs) {
    return LearnSessionDto.builder()
        .sessionId(UUID.randomUUID().toString())
        .startedAt(LocalDateTime.now())
        .questions(qs)
        .build();
  }
}
