package com.OhRyue.certpilot.versus.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "tournament_bracket")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TournamentBracket {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "room_id", nullable = false)
  private Long roomId;

  @Column(name = "round_no", nullable = false)
  private Integer roundNo;

  @Column(name = "pairing_json", nullable = false, columnDefinition = "JSON")
  private String pairingJson;
}
 