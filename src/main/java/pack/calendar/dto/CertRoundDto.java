package pack.calendar.dto;

import pack.calendar.RoundStatus;

import java.time.LocalDate;

public record CertRoundDto(
    Long scheduleId,
    Long certId,
    String certName,      // ← 추가: 조인으로 가져올 자격증 이름
    int year,
    int term,
    LocalDate regStart,
    LocalDate regEnd,
    LocalDate examDate,
    RoundStatus status,
    Integer daysUntilExam // 시험까지 D-day (과거면 음수/또는 null 처리 가능)
) {}
