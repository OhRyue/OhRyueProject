package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(
        name = "concept",
        uniqueConstraints = {
                @UniqueConstraint(name = "uq_concept_topic", columnNames = "topic_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Concept {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // topic(id) FK, 숫자 FK로만 관리
    @Column(name = "topic_id", nullable = false)
    private Long topicId;

    /**
     * sections_json 컬럼
     * - 리치 블록(섹션 배열)을 JSON 문자열로 저장
     * - 실제 구조는 study-service의 ConceptMapper 또는 프론트에서 파싱
     */
    @Column(name = "sections_json", nullable = false, columnDefinition = "JSON")
    private String sectionsJson;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Instant createdAt;

    @Column(name = "updated_at", nullable = false)
    private Instant updatedAt;

    @PrePersist
    void onCreate() {
        Instant now = Instant.now();
        if (createdAt == null) {
            createdAt = now;
        }
        if (updatedAt == null) {
            updatedAt = now;
        }
    }

    @PreUpdate
    void onUpdate() {
        updatedAt = Instant.now();
    }
}
