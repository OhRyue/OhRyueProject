package com.OhRyue.certpilot.calendar.dto;

import com.OhRyue.certpilot.calendar.entity.RoundStatus;

import java.time.LocalDate;

public record CertRoundDto(
    Long scheduleId,
    Long certId,
    String certName,
    int year,
    int term,
    LocalDate regStart,
    LocalDate regEnd,
    LocalDate examDate,
    RoundStatus status,
    Integer daysUntilExam
) {}

