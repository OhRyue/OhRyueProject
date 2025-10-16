package com.OhRyue.certpilot.question.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "question")
public class Question {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String source; // custom/year
    private Integer year;
    private Integer round;
    private Integer difficulty; // 1~3

    @Column(columnDefinition = "TEXT", nullable = false)
    private String stem;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "choices_json", columnDefinition = "JSON", nullable = false)
    private String choicesJson;

    @Column(name = "answer_idx", nullable = false)
    private Integer answerIdx;

    @Column(columnDefinition = "TEXT")
    private String exp;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "meta_json", columnDefinition = "JSON")
    private String metaJson;
}
