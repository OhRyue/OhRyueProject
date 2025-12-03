package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "qnet_qualification_info")
@Getter
@Setter
public class QnetQualificationInfoEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "jm_cd", length = 20, nullable = false)
    private String jmCd;

    @Column(name = "infogb", length = 50)
    private String infogb; // 출제경향, 출제기준, 취득방법

    @Column(name = "contents", columnDefinition = "TEXT")
    private String contents;

    @Column(name = "jmfldnm", length = 255)
    private String jmfldnm;

    @Column(name = "mdobligfldcd", length = 20)
    private String mdobligfldcd;

    @Column(name = "mdobligfldnm", length = 255)
    private String mdobligfldnm;

    @Column(name = "obligfldcd", length = 20)
    private String obligfldcd;

    @Column(name = "obligfldnm", length = 255)
    private String obligfldnm;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Instant createdAt;

    @Column(name = "updated_at", nullable = false)
    private Instant updatedAt;

    @PrePersist
    void onCreate() {
        Instant now = Instant.now();
        this.createdAt = now;
        this.updatedAt = now;
    }

    @PreUpdate
    void onUpdate() {
        this.updatedAt = Instant.now();
    }
}

