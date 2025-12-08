package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "battle_record", indexes = {
    @Index(name = "ix_br_user_time", columnList = "user_id,completed_at"),
    @Index(name = "ix_br_room", columnList = "room_id"),
    @Index(name = "ix_br_mode", columnList = "mode")
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BattleRecord {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false, length = 100)
    private String userId;

    @Column(name = "room_id", nullable = false)
    private Long roomId;

    @Column(name = "mode", nullable = false, length = 20)
    private String mode; // DUEL, TOURNAMENT, GOLDENBELL

    @Enumerated(EnumType.STRING)
    @Column(name = "exam_mode", length = 20)
    private ExamMode examMode;

    @Column(name = "score", nullable = false)
    private Integer score;

    @Column(name = "player_rank", nullable = false)
    private Integer rank;

    @Column(name = "correct_count", nullable = false)
    private Integer correctCount;

    @Column(name = "total_count", nullable = false)
    private Integer totalCount;

    @Column(name = "total_time_ms", nullable = false)
    private Long totalTimeMs;

    @Column(name = "is_winner", nullable = false)
    private Boolean isWinner;

    @Column(name = "completed_at", nullable = false)
    private Instant completedAt;

    @PrePersist
    void prePersist() {
        if (completedAt == null) {
            completedAt = Instant.now();
        }
    }
}

