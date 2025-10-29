package com.OhRyue.certpilot.curriculum.entity;

import jakarta.persistence.*;
import lombok.*;

@Getter
@Setter
@Entity
@Table(name = "concept_check")
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ConceptCheck {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "concept_id", nullable = false)
    private Long conceptId;

    @Column(nullable = false, length = 500)
    private String stem;

    @Column(name = "choices_json", columnDefinition = "json", nullable = false)
    private String choicesJson;

    @Column(name = "answer_idx", nullable = false)
    private Integer answerIdx;

    @Column(name = "explanation", length = 1000)
    private String explanation;

    @Column(name = "is_ox", nullable = false)
    @Builder.Default
    private boolean isOx = true;

    @Column(name = "ox_answer")
    private Integer oxAnswer;
}

