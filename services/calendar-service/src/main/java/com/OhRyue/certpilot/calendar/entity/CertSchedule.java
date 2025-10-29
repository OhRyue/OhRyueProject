package com.OhRyue.certpilot.calendar.entity;

import jakarta.persistence.*;
import java.time.LocalDate;

@Entity
@Table(name = "cert_schedule",
    indexes = {
        @Index(name = "idx_cert_year", columnList = "cert_id,year")
    })
public class CertSchedule {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "cert_id", nullable = false)
  private Long certId;

  @Column(nullable = false)
  private Integer term;

  @Column(nullable = false)
  private Integer year;

  private LocalDate regStart;
  private LocalDate regEnd;
  private LocalDate examDate;

  protected CertSchedule() {}

  public CertSchedule(Long certId, Integer year, Integer term,
                      LocalDate regStart, LocalDate regEnd, LocalDate examDate) {
    this.certId = certId;
    this.year = year;
    this.term = term;
    this.regStart = regStart;
    this.regEnd = regEnd;
    this.examDate = examDate;
  }

  public Long getId() { return id; }
  public Long getCertId() { return certId; }
  public Integer getTerm() { return term; }
  public Integer getYear() { return year; }
  public LocalDate getRegStart() { return regStart; }
  public LocalDate getRegEnd() { return regEnd; }
  public LocalDate getExamDate() { return examDate; }

  public void setCertId(Long certId) { this.certId = certId; }
  public void setTerm(Integer term) { this.term = term; }
  public void setYear(Integer year) { this.year = year; }
  public void setRegStart(LocalDate regStart) { this.regStart = regStart; }
  public void setRegEnd(LocalDate regEnd) { this.regEnd = regEnd; }
  public void setExamDate(LocalDate examDate) { this.examDate = examDate; }
}

