package com.OhRyue.certpilot.shared.dto;

import lombok.*;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuestionDto {
  private Long id;
  private String stem;
  private List<String> choices;
  private Integer difficulty;
}
