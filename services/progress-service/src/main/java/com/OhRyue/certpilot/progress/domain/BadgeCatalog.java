package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.Rarity;
import jakarta.persistence.*;
import lombok.*;

@Entity @Table(name="badge_catalog")
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class BadgeCatalog {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable=false, unique=true, length=100)
  private String code;

  @Column(nullable=false)
  private String name;

  @Column(columnDefinition="TEXT")
  private String description;

  @Column(length=50)
  private String category;

  @Enumerated(EnumType.STRING)
  @Column(nullable=false, length=8)
  private Rarity rarity;

  @Column(name="icon_url", length=500)
  private String iconUrl;

  @Column(columnDefinition="JSON")
  private String ruleJson;
}
