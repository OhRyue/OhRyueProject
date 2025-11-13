package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "goldenbell_rule")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GoldenbellRule {

  @Id
  @Column(name = "room_id")
  private Long roomId;

  @Column(name = "round_flow_json", nullable = false, columnDefinition = "JSON")
  private String roundFlowJson;

  @Enumerated(EnumType.STRING)
  @Column(name = "elimination", nullable = false, length = 32)
  private GoldenbellElimination elimination;

  @Column(name = "revival_rule_json", columnDefinition = "JSON")
  private String revivalRuleJson;
}
 