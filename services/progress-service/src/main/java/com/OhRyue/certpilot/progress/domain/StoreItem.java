package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity @Table(name="store_item")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class StoreItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable=false)
  private String name;

  @Column(nullable=false)
  private int price;

  private Integer limitPerUser;

  @Column(nullable=false)
  private boolean isActive;
}
