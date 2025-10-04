package pack.calendar.dto;

import pack.calendar.CertSchedule;

import java.time.LocalDate;

public class CertScheduleDto {
  private final Long id;
  private final Integer year;
  private final Integer term;
  private final LocalDate regStart;
  private final LocalDate regEnd;
  private final LocalDate examDate;

  public CertScheduleDto(Long id, Integer year, Integer term,
                         LocalDate regStart, LocalDate regEnd, LocalDate examDate) {
    this.id = id;
    this.year = year;
    this.term = term;
    this.regStart = regStart;
    this.regEnd = regEnd;
    this.examDate = examDate;
  }

  public static CertScheduleDto from(CertSchedule s) {
    return new CertScheduleDto(
        s.getId(), s.getYear(), s.getTerm(),
        s.getRegStart(), s.getRegEnd(), s.getExamDate()
    );
  }

  public Long getId() { return id; }
  public Integer getYear() { return year; }
  public Integer getTerm() { return term; }
  public LocalDate getRegStart() { return regStart; }
  public LocalDate getRegEnd() { return regEnd; }
  public LocalDate getExamDate() { return examDate; }
}
