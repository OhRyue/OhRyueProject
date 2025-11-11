package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity @Table(name="user_loadout")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserLoadout {
  @Id @Column(length=100)
  private String userId;

  private Long hatId;
  private Long clothesId;
  private Long accId;
  private Long bgId;
  private Long specialId;
}
