package com.OhRyue.certpilot.progress.domain;

import lombok.*;
import java.io.Serializable;
import java.time.LocalDate;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor @EqualsAndHashCode
public class ReportDailyKey implements Serializable {
  private String userId;
  private LocalDate date;
}
