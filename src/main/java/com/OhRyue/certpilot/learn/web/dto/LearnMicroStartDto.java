package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnMicroStartDto(
    Long conceptId,
    String conceptTitle,
    String conceptSummary,
    String conceptPitfalls,
    List<Mini> miniChecks,
    List<Quiz> quiz // 5문항
) {
  public record Mini(Long id, String stem, List<String> choices) {}
  public record Quiz(Long id, String stem, List<String> choices, Integer difficulty) {}
}
