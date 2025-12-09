package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public class ReviewDtos {

  @Schema(description = "리뷰(총정리) 한 문제")
  public record ReviewQuestion(
      Long questionId,
      String text,
      List<Choice> choices,
      String imageUrl,
      @Schema(description = "태그 목록") List<com.OhRyue.common.dto.TagViewDto> tags
  ) {
    public record Choice(String label, String text) {}
  }

  @Schema(description = "리뷰(총정리) 세트")
  public record ReviewSet(List<ReviewQuestion> items) {}
}
