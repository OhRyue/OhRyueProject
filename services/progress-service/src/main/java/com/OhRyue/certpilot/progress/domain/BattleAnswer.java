package com.OhRyue.certpilot.progress.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "battle_answer", indexes = {
    @Index(name = "ix_ba_battle", columnList = "battle_record_id"),
    @Index(name = "ix_ba_question", columnList = "question_id")
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BattleAnswer {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "battle_record_id", nullable = false)
    private BattleRecord battleRecord;

    @Column(name = "question_id", nullable = false)
    private Long questionId;

    @Column(name = "user_answer", columnDefinition = "TEXT")
    private String userAnswer;

    @Column(name = "is_correct", nullable = false)
    private Boolean isCorrect;

    @Column(name = "time_ms", nullable = false)
    private Integer timeMs;

    @Column(name = "score_delta", nullable = false)
    private Integer scoreDelta;

    @Column(name = "round_no")
    private Integer roundNo;

    @Column(name = "phase", length = 20)
    private String phase;

    @Column(name = "submitted_at", nullable = false)
    private Instant submittedAt;

    @PrePersist
    void prePersist() {
        if (submittedAt == null) {
            submittedAt = Instant.now();
        }
    }
}

