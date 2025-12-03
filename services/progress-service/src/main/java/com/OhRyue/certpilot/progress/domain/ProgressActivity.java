package com.OhRyue.certpilot.progress.domain;

import com.OhRyue.certpilot.progress.domain.enums.ActivityGroup;
import com.OhRyue.certpilot.progress.domain.enums.AssistType;
import com.OhRyue.certpilot.progress.domain.enums.BattleType;
import com.OhRyue.certpilot.progress.domain.enums.ExamMode;
import com.OhRyue.certpilot.progress.domain.enums.MainType;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.math.BigDecimal;


@Entity
@Table(
    name = "progress_activity",
    indexes = {
        @Index(name = "ix_pa_user_finished", columnList = "user_id, finished_at"),
        @Index(name = "ix_pa_user_started", columnList = "user_id, started_at"),
        @Index(name = "ix_pa_group", columnList = "activity_group"),
        @Index(name = "ix_pa_source", columnList = "source_service, source_session_id")
    }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProgressActivity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false, length = 100)
    private String userId;

    @Enumerated(EnumType.STRING)
    @Column(name = "activity_group", nullable = false, length = 20)
    private ActivityGroup activityGroup;

    @Enumerated(EnumType.STRING)
    @Column(name = "main_type", length = 20)
    private MainType mainType;  // MAIN일 때만 사용

    @Enumerated(EnumType.STRING)
    @Column(name = "assist_type", length = 20)
    private AssistType assistType;  // ASSIST일 때만 사용

    @Enumerated(EnumType.STRING)
    @Column(name = "battle_type", length = 20)
    private BattleType battleType;  // BATTLE일 때만 사용

    @Enumerated(EnumType.STRING)
    @Column(name = "mode", nullable = false, length = 20)
    private ExamMode mode;  // WRITTEN / PRACTICAL

    @Column(name = "topic_id")
    private Long topicId;  // 토픽 기반 모드에서 사용

    @Column(name = "topic_name", length = 255)
    private String topicName;  // 캐싱용(리포트 빠르게 조회)

    @Column(name = "weakness_tag_name", length = 100)
    private String weaknessTagName;  // 약점보완 모드에서 사용

    @Column(name = "difficulty", length = 20)
    private String difficulty;  // 난이도 모드에서 사용 (EASY/NORMAL/HARD)

    @Column(name = "question_count", nullable = false)
    private Integer questionCount;

    @Column(name = "correct_count", nullable = false)
    private Integer correctCount;

    @Column(name = "accuracy_pct", nullable = false, precision = 5, scale = 2)
    private BigDecimal accuracyPct;

    @Column(name = "final_rank")
    private Integer finalRank;  // 토너먼트/골든벨 최종 순위

    @Column(name = "xp_gained")
    private Integer xpGained;  // 이 활동으로 획득한 XP

    // 원본 서비스/세션 식별용
    @Column(name = "source_service", nullable = false, length = 50)
    private String sourceService;  // "study", "versus"

    @Column(name = "source_session_id", nullable = false)
    private Long sourceSessionId;  // study-session-id 또는 match-id

    @Column(name = "started_at", nullable = false)
    private LocalDateTime startedAt;

    @Column(name = "finished_at", nullable = false)
    private LocalDateTime finishedAt;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @PrePersist
    void prePersist() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
    }
}

