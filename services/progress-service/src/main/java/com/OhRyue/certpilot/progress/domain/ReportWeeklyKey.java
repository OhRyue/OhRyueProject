package com.OhRyue.certpilot.progress.domain;

import lombok.*;
import java.io.Serializable;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor @EqualsAndHashCode
public class ReportWeeklyKey implements Serializable {
  private String userId;
  private String weekIso;
}
