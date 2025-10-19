package com.OhRyue.certpilot.concept.domain;

import jakarta.persistence.*;
import lombok.*;

@Getter
@Setter
@Entity
@Table(name = "concept_check")
@NoArgsConstructor @AllArgsConstructor @Builder
public class ConceptCheck {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "concept_id", nullable = false)
    private Long conceptId;

    @Column(nullable = false, length = 500)
    private String stem;

    // MySQL JSON 컬럼 → 엔티티에서는 String으로 들고 오고, 서비스에서 List로 변환
    @Column(name = "choices_json", columnDefinition = "json", nullable = false)
    private String choicesJson;

    @Column(name = "answer_idx", nullable = false)
    private Integer answerIdx;

    @Column(name = "explanation", length = 1000)
    private String explanation;

    @Column(name = "is_ox", nullable = false)
    @Builder.Default
    private boolean isOx = true;

    // 1 = O(true), 0 = X(false)
    @Column(name = "ox_answer")
    private Integer oxAnswer;
}
