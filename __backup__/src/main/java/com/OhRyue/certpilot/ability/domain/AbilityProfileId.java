package com.OhRyue.certpilot.ability.domain;

import java.io.Serializable;
import java.util.Objects;

public class AbilityProfileId implements Serializable {
  private Long userId;
  private String tag;

  public AbilityProfileId() {}
  public AbilityProfileId(Long userId, String tag) {
    this.userId = userId;
    this.tag = tag;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof AbilityProfileId that)) return false;
    return Objects.equals(userId, that.userId) && Objects.equals(tag, that.tag);
  }

  @Override
  public int hashCode() {
    return Objects.hash(userId, tag);
  }
}
