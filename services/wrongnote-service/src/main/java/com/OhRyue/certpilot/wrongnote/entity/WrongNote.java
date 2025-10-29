package com.OhRyue.certpilot.wrongnote.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Getter
@NoArgsConstructor
@Entity
@Table(
        name = "wrong_note",
        indexes = {
                @Index(name = "idx_wrong_note_user_status", columnList = "user_id, status, last_wrong_at DESC")
        }
)
@IdClass(WrongNoteId.class)
public class WrongNote {

    @Id
    @Column(name = "user_id", nullable = false)
    private Long userId;

    @Id
    @Column(name = "question_id", nullable = false)
    private Long questionId;

    @Id
    @Column(name = "tag", nullable = false, length = 100)
    private String tag;

    @Column(name = "wrong_count", nullable = false)
    private Integer wrongCount = 0;

    @Column(name = "first_wrong_at")
    private Instant firstWrongAt;

    @Column(name = "last_wrong_at")
    private Instant lastWrongAt;

    @Enumerated(EnumType.STRING)
    @Column(name = "status", columnDefinition = "ENUM('todo','reviewing','cleared')")
    private WrongNoteStatus status = WrongNoteStatus.todo;

    public WrongNote(Long userId, Long questionId, String tag) {
        this.userId = userId;
        this.questionId = questionId;
        this.tag = tag;
        this.wrongCount = 0;
        this.status = WrongNoteStatus.todo;
    }

    public void markWrongOnce() {
        this.wrongCount = this.wrongCount + 1;
        Instant now = Instant.now();
        if (this.firstWrongAt == null) this.firstWrongAt = now;
        this.lastWrongAt = now;
        if (this.status == null) this.status = WrongNoteStatus.todo;
    }

    public void setStatus(WrongNoteStatus status) {
        this.status = status;
    }
}

