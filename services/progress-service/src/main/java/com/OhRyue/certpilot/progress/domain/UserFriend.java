// src/main/java/com/OhRyue/certpilot/progress/domain/UserFriend.java
package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity @Table(name="user_friend", indexes = {
    @Index(name="ix_friend_user", columnList="user_id"),
    @Index(name="ix_friend_friend", columnList="friend_id"),
    @Index(name="uq_friend_pair", columnList="user_id,friend_id", unique = true)
})
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class UserFriend {
  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name="user_id", length=100, nullable=false)
  private String userId;

  @Column(name="friend_id", length=100, nullable=false)
  private String friendId;

  @Column(nullable=false, length=16)
  private String status;
}
