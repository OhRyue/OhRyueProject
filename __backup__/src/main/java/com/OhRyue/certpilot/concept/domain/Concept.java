package com.OhRyue.certpilot.concept.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

@Getter
@NoArgsConstructor
@Entity
@Table(name = "concept")
public class Concept {
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "cert_id", nullable = false)
    private Long certId;

    @Column(nullable = false, length = 100)
    private String category;

    @Column(nullable = false, length = 200)
    private String title;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String summary;

    @Column(columnDefinition = "TEXT")
    private String pitfalls;

    @Column(name = "topic_id")
    private Long topicId;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "examples_json", columnDefinition = "JSON")
    private String examplesJson;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "tags_json", columnDefinition = "JSON")
    private String tagsJson;

    public Concept(Long certId, String category, String title, String summary, String pitfalls, String examplesJson, String tagsJson) {
        this.certId = certId;
        this.category = category;
        this.title = title;
        this.summary = summary;
        this.pitfalls = pitfalls;
        this.examplesJson = examplesJson;
        this.tagsJson = tagsJson;
    }

    public Long getTopicId() { return topicId; }
}
