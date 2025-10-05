package pack.calendar.dto;

import java.time.LocalDate;

public interface RoundWithCert {
  Long getId();
  Long getCertId();
  Integer getYear();
  Integer getTerm();
  LocalDate getRegStart();
  LocalDate getRegEnd();
  LocalDate getExamDate();
  String getCertName();
}
