package com.OhRyue.certpilot.question.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@Entity
@Table(name="question_tag")
@IdClass(QuestionTagId.class)
public class QuestionTag {
    @Id @Column(name="question_id") private Long questionId;
    @Id private String tag;
}
