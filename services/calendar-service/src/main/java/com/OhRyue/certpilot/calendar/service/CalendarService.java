package com.OhRyue.certpilot.calendar.service;

import com.OhRyue.certpilot.calendar.client.CertsClient;
import com.OhRyue.certpilot.calendar.dto.CertNameDto;
import com.OhRyue.certpilot.calendar.dto.CertRoundDto;
import com.OhRyue.certpilot.calendar.dto.FullCalendarEventDto;
import com.OhRyue.certpilot.calendar.entity.RoundStatus;
import com.OhRyue.certpilot.calendar.repository.CertScheduleRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CalendarService {

  private final CertScheduleRepository scheduleRepo;
  private final CertsClient certsClient;
  private static final ZoneId ZONE = ZoneId.of("Asia/Seoul");

  public List<CertRoundDto> getRounds(Long certId, Integer year) {
    var rows = scheduleRepo.findRounds(certId, year);
    var today = LocalDate.now(ZONE);

    return rows.stream()
        .map(r -> {
          // Fetch cert name from certs-service
          CertNameDto certDto;
          try {
            certDto = certsClient.getById(r.getCertId());
          } catch (Exception e) {
            certDto = new CertNameDto(r.getCertId(), "자격증 정보 없음", null);
          }

          var status = computeStatus(today, r.getRegStart(), r.getRegEnd(), r.getExamDate());
          Integer dday = (r.getExamDate() == null) ? null
              : (int) ChronoUnit.DAYS.between(today, r.getExamDate());

          return new CertRoundDto(
              r.getId(),
              r.getCertId(),
              certDto.getName(),
              r.getYear(),
              r.getTerm(),
              r.getRegStart(),
              r.getRegEnd(),
              r.getExamDate(),
              status,
              dday
          );
        })
        .toList();
  }

  public List<FullCalendarEventDto> getMyEvents(Long certId, Integer year) {
    var rounds = getRounds(certId, year);
    return rounds.stream()
        .filter(r -> r.examDate() != null)
        .map(r -> {
          var title = String.format("%s %d년 %d회", r.certName(), r.year(), r.term());
          return new FullCalendarEventDto("round-" + r.scheduleId(), title, r.examDate().toString());
        })
        .toList();
  }

  private RoundStatus computeStatus(LocalDate today, LocalDate regStart, LocalDate regEnd, LocalDate examDate) {
    if (examDate != null && today.isAfter(examDate)) return RoundStatus.DONE;
    if (examDate != null && today.isEqual(examDate)) return RoundStatus.EXAM_TODAY;
    if (regStart != null && today.isBefore(regStart)) return RoundStatus.BEFORE_REG;
    if (regStart != null && regEnd != null && (!today.isBefore(regStart) && !today.isAfter(regEnd)))
      return RoundStatus.REGISTER_OPEN;
    if (regEnd != null && examDate != null && today.isAfter(regEnd) && today.isBefore(examDate))
      return RoundStatus.BEFORE_EXAM;
    if (examDate != null && today.isBefore(examDate)) return RoundStatus.BEFORE_EXAM;
    return RoundStatus.BEFORE_REG;
  }
}

