// src/main/java/com/OhRyue/certpilot/progress/domain/UserInventory.java
package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;

@Entity @Table(name="user_inventory", indexes = {
    @Index(name="ix_inv_user", columnList="user_id"),
    @Index(name="ix_inv_item", columnList="item_id"),
    @Index(name="uq_inventory_user_item", columnList="user_id,item_id", unique = true)
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserInventory {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(name="item_id", nullable=false)
  private Long itemId;

  @Column(name="owned_at", nullable=false)
  private Instant ownedAt;

  @PrePersist void pre(){ if(ownedAt==null) ownedAt=Instant.now(); }
}
