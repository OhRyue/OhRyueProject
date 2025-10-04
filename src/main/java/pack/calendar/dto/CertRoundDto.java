package pack.calendar.dto;

import pack.calendar.RoundStatus;

import java.time.LocalDate;

public record CertRoundDto(
    Long scheduleId,
    Long certId,
    int year,
    int term,
    LocalDate regStart,
    LocalDate regEnd,
    LocalDate examDate,
    RoundStatus status,
    Integer daysUntilExam // 시험까지 D-day (과거면 음수/또는 null 처리 가능)
) {}
