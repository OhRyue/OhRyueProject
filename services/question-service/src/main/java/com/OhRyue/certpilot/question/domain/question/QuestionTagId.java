package com.OhRyue.certpilot.question.domain.question;

import java.io.Serializable;
import java.util.Objects;

public class QuestionTagId implements Serializable {
  private Long questionId;
  private String tag;

  public QuestionTagId() {}
  public QuestionTagId(Long questionId, String tag) {
    this.questionId = questionId; this.tag = tag;
  }

  @Override public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof QuestionTagId that)) return false;
    return Objects.equals(questionId, that.questionId) &&
        Objects.equals(tag, that.tag);
  }
  @Override public int hashCode() { return Objects.hash(questionId, tag); }
}
