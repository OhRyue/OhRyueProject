package com.OhRyue.certpilot.cert.domain;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.Instant;
import java.time.LocalDateTime;

@Entity
@Table(name = "open_question")
@Getter
@Setter
public class OpenQuestionEntity {

  @Id
  @Column(name = "artl_seq")
  private Long artlSeq;

  @Column(name = "title", length = 500)
  private String title;

  @Column(name = "series_cd", length = 10)
  private String seriesCd;

  @Column(name = "series_nm")
  private String seriesNm;

  @Column(name = "qualgb_cd", length = 10)
  private String qualgbCd;

  @Column(name = "qualgb_nm")
  private String qualgbNm;

  @Column(name = "jm_cd", length = 20)
  private String jmCd;

  @Column(name = "jm_nm")
  private String jmNm;

  @Column(name = "reg_dttm")
  private LocalDateTime regDttm;

  @Column(name = "mod_dttm")
  private LocalDateTime modDttm;

  @Column(name = "attachments_json", columnDefinition = "JSON")
  private String attachmentsJson;

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

