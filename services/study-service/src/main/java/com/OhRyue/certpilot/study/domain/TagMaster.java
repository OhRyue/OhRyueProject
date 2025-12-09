package com.OhRyue.certpilot.study.domain;

import com.OhRyue.certpilot.study.domain.enums.TagDomain;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
    name = "tag_master",
    indexes = {
        @Index(name = "ix_tag_domain", columnList = "domain, code")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TagMaster {

  @Id
  @Column(nullable = false, length = 100)
  private String code;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, length = 16)
  private TagDomain domain;

  @Column(name = "label_ko", nullable = false, length = 255)
  private String labelKo;

  @Column(name = "label_en", length = 255)
  private String labelEn;

  @Lob
  @Column(name = "description", columnDefinition = "TEXT")
  private String description;

  @Column(name = "order_no")
  private Integer orderNo;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = Instant.now();
    }
  }
}



