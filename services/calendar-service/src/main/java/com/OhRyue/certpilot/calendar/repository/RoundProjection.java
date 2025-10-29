package com.OhRyue.certpilot.calendar.repository;

import java.time.LocalDate;

public interface RoundProjection {
  Long getId();
  Long getCertId();
  Integer getYear();
  Integer getTerm();
  LocalDate getRegStart();
  LocalDate getRegEnd();
  LocalDate getExamDate();
}

