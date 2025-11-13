package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;

@Entity
@Table(name = "qualification")
@Getter
@Setter
public class QualificationEntity {

  @Id
  @Column(name = "jm_cd", length = 20)
  private String jmCd;

  @Column(name = "series_cd", length = 10)
  private String seriesCd;

  @Column(name = "jm_nm")
  private String jmNm;

  @Column(name = "eng_jm_nm")
  private String engJmNm;

  @Column(name = "series_nm")
  private String seriesNm;

  @Column(name = "impl_nm")
  private String implNm;

  @Column(name = "insti_nm")
  private String instiNm;

  @Column(name = "summary", columnDefinition = "TEXT")
  private String summary;

  @Column(name = "job", columnDefinition = "TEXT")
  private String job;

  @Column(name = "trend", columnDefinition = "TEXT")
  private String trend;

  @Column(name = "career", columnDefinition = "TEXT")
  private String career;

  @Column(name = "hist", columnDefinition = "TEXT")
  private String hist;

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

