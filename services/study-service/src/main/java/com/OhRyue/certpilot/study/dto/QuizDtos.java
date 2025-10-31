package com.OhRyue.certpilot.study.dto;

import com.OhRyue.certpilot.study.domain.enums.Difficulty;
import jakarta.validation.constraints.NotNull;

import java.util.List;

public class QuizDtos {

  public record CategoryStartReq(@NotNull String userId, List<Long> topicIds, int count) {}
  public record DifficultyStartReq(@NotNull String userId, @NotNull Difficulty difficulty, int count) {}
  public record WeaknessStartReq(@NotNull String userId, int count) {}

  public record QuizQ(Long id, String text, List<McqChoice> choices) {
    public record McqChoice(String label, String text) {}
  }
  public record QuizSet(List<QuizQ> questions) {}
}
