package com.OhRyue.certpilot.progress.domain;

import lombok.*;
import java.io.Serializable;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor @EqualsAndHashCode
public class AssistWeeklyKey implements Serializable {
  private String userId;
  private String weekIso;
}
