package com.OhRyue.certpilot.account.dto;

import com.OhRyue.certpilot.account.domain.ExamMode;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDate;

public class GoalCertDtos {

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class GoalResponse {
    private Long id;
    private String userId;
    private Long certId;
    private ExamMode targetExamMode;
    private Long targetRoundId;
    private LocalDate targetExamDate;
    private Integer ddayCached;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class GoalUpsertRequest {
    @NotNull
    private Long certId;

    @NotNull
    private ExamMode targetExamMode;

    private Long targetRoundId;

    private LocalDate targetExamDate;
  }

  @Getter
  @Setter
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class GoalDateUpdateRequest {
    @NotNull
    private LocalDate targetExamDate;
  }
}
