package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.ItemCategory;
import com.OhRyue.certpilot.progress.domain.enums.Rarity;
import jakarta.persistence.*;
import lombok.*;

@Entity @Table(name="store_item")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class StoreItem {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Enumerated(EnumType.STRING) @Column(nullable=false, length=10)
  private ItemCategory category;

  @Column(nullable=false)
  private String name;

  private String imageUrl;

  @Column(nullable=false)
  private int price;

  @Enumerated(EnumType.STRING) @Column(nullable=false, length=8)
  private Rarity rarity;

  private Integer limitPerUser;

  @Column(nullable=false)
  private boolean isActive;
}
