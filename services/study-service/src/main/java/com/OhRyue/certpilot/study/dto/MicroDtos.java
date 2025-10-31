package com.OhRyue.certpilot.study.dto;

import jakarta.validation.constraints.NotNull;
import java.util.List;

public class MicroDtos {

  public record StartRequest(@NotNull String userId, @NotNull Long topicId) {}
  public record ConceptResp(Long topicId, String title, String content) {}

  /* Mini (OX) */
  public record MiniQuestion(Long id, String text) {}
  public record MiniSet(List<MiniQuestion> questions) {}

  public record MiniAnswer(Long questionId, boolean answer) {}

  public record MiniSubmitReq(@NotNull String userId, @NotNull Long topicId,
                              List<MiniAnswer> answers) {}

  public record MiniSubmitItem(Long questionId, boolean correct,
                               String explanation, String aiExplanation) {}

  public record MiniSubmitResp(int total, int correct, boolean passed,
                               List<MiniSubmitItem> items) {}

  /* MCQ */
  public record McqChoice(String label, String text) {}
  public record McqQuestion(Long id, String text, List<McqChoice> choices, String imageUrl) {}
  public record McqSet(List<McqQuestion> questions) {}

  public record McqAnswer(Long questionId, String label) {}
  public record McqSubmitReq(@NotNull String userId, @NotNull Long topicId,
                             List<McqAnswer> answers) {}

  public record McqSubmitItem(Long questionId, boolean correct,
                              String correctLabel, String explanation, String aiExplanation) {}

  public record McqSubmitResp(int total, int correct, List<McqSubmitItem> items) {}

  public record SummaryResp(int miniTotal, int miniCorrect, boolean miniPassed,
                            int mcqTotal, int mcqCorrect) {}
}
