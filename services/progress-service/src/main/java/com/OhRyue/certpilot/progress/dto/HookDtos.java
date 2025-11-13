package com.OhRyue.certpilot.progress.dto;

import jakarta.validation.constraints.NotBlank;

import java.util.List;

public class HookDtos {

  public record StudySubmitReq(
      @NotBlank String userId,
      @NotBlank String examMode,
      @NotBlank String questionType,
      Boolean correct,
      Integer score,
      List<String> tags,
      String source
  ) {}
}


