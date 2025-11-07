package com.OhRyue.certpilot.profile.domain;

import com.OhRyue.certpilot.profile.domain.enums.Difficulty;
import com.OhRyue.certpilot.profile.domain.enums.QuestionType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
@Table(name = "question",
        indexes = {
                @Index(name = "ix_q_topic", columnList = "topicId"),
                @Index(name = "ix_q_type", columnList = "type"),
                @Index(name = "ix_q_diff", columnList = "difficulty")
        })
public class Question {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Long topicId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private QuestionType type; // OX / MCQ / SHORT / LONG

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Difficulty difficulty;

    @Column(columnDefinition = "TEXT")
    private String text;

    private String imageUrl;

    // OX 전용
    private Boolean oxAnswer;

    @Column(columnDefinition = "TEXT")
    private String explanation;
}
