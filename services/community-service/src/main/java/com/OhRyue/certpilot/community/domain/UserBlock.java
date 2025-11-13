package com.OhRyue.certpilot.community.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.PrePersist;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.Instant;
import java.util.Objects;

@Entity
@Table(name = "user_block")
@IdClass(UserBlock.UserBlockId.class)
@Getter
@Setter
public class UserBlock {

  @Id
  @Column(name = "user_id", length = 100, nullable = false)
  private String userId;

  @Id
  @Column(name = "blocked_user_id", length = 100, nullable = false)
  private String blockedUserId;

  @Column(name = "created_at", nullable = false, updatable = false)
  private Instant createdAt;

  @PrePersist
  void onCreate() {
    createdAt = Instant.now();
  }

  @Getter
  @Setter
  public static class UserBlockId implements Serializable {
    private String userId;
    private String blockedUserId;

    public UserBlockId() {}

    public UserBlockId(String userId, String blockedUserId) {
      this.userId = userId;
      this.blockedUserId = blockedUserId;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;
      UserBlockId that = (UserBlockId) o;
      return Objects.equals(userId, that.userId) && Objects.equals(blockedUserId, that.blockedUserId);
    }

    @Override
    public int hashCode() {
      return Objects.hash(userId, blockedUserId);
    }
  }
}

