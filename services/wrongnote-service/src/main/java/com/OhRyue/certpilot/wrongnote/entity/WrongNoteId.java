package com.OhRyue.certpilot.wrongnote.entity;

import java.io.Serializable;
import java.util.Objects;

public class WrongNoteId implements Serializable {
  private Long userId;
  private Long questionId;
  private String tag;

  public WrongNoteId() {}
  public WrongNoteId(Long userId, Long questionId, String tag) {
    this.userId = userId;
    this.questionId = questionId;
    this.tag = tag;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof WrongNoteId that)) return false;
    return Objects.equals(userId, that.userId) && 
           Objects.equals(questionId, that.questionId) && 
           Objects.equals(tag, that.tag);
  }

  @Override
  public int hashCode() {
    return Objects.hash(userId, questionId, tag);
  }
}

