package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

public class WrongReviewDtos {

  @Schema(description = "오답복습 한 문제")
  public record WrongQuestion(
      Long questionId,
      String type,           // OX | MCQ
      String text,
      List<Choice> choices,  // MCQ일 때만
      String imageUrl
  ) {
    public record Choice(String label, String text) {}
  }

  @Schema(description = "오답복습 세트")
  public record WrongSet(List<WrongQuestion> items) {}
}
