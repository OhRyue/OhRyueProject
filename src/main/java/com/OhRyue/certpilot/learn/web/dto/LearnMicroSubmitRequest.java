package com.OhRyue.certpilot.learn.web.dto;

import java.util.List;

public record LearnMicroSubmitRequest(
    Long userId,
    List<MiniAnswer> miniAnswers,   // concept_check 기준
    List<QuizAnswer> quizAnswers    // question 기준
) {
  public record MiniAnswer(Long id, Integer choiceIdx) {}
  public record QuizAnswer(Long id, Integer choiceIdx) {}
}
