package com.OhRyue.certpilot.account.dto;

import com.OhRyue.certpilot.account.domain.ExamMode;
import lombok.*;

public class GoalCertDtos {

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class GoalResponse {
    private Long id;
    private String userId;
    private Long certId;
    private ExamMode targetExamMode;
    private Long targetRoundId;
    private Integer ddayCached;
  }

  @Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
  public static class GoalUpsertRequest {
    private Long certId;
    private ExamMode targetExamMode;  // WRITTEN / PRACTICAL
    private Long targetRoundId;       // null 허용
  }
}
