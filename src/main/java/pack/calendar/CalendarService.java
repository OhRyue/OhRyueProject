package pack.calendar;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import pack.calendar.dto.CertRoundDto;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CalendarService {

  private final CertScheduleRepository scheduleRepo;
  private static final ZoneId ZONE = ZoneId.of("Asia/Seoul");

  public List<CertRoundDto> getRounds(Long certId, Integer year) {
    var schedules = (year == null)
        ? scheduleRepo.findByCertIdOrderByExamDateAsc(certId)
        : scheduleRepo.findByCertIdAndYearOrderByExamDateAsc(certId, year);

    var today = LocalDate.now(ZONE);

    return schedules.stream()
        .map(s -> {
          var status = computeStatus(today, s.getRegStart(), s.getRegEnd(), s.getExamDate());
          Integer dday = null;
          if (s.getExamDate() != null) {
            dday = (int) ChronoUnit.DAYS.between(today, s.getExamDate());
          }
          return new CertRoundDto(
              s.getId(),
              s.getCertId(),
              s.getYear(),
              s.getTerm(),
              s.getRegStart(),
              s.getRegEnd(),
              s.getExamDate(),
              status,
              dday
          );
        })
        .toList();
  }

  private RoundStatus computeStatus(LocalDate today, LocalDate regStart, LocalDate regEnd, LocalDate examDate) {
    // 날짜가 일부 null일 수 있는 경우를 안전하게 처리
    if (examDate != null && today.isAfter(examDate)) {
      return RoundStatus.DONE;
    }
    if (examDate != null && today.isEqual(examDate)) {
      return RoundStatus.EXAM_TODAY;
    }
    if (regStart != null && today.isBefore(regStart)) {
      return RoundStatus.BEFORE_REG;
    }
    if (regStart != null && regEnd != null && ( !today.isBefore(regStart) && !today.isAfter(regEnd) )) {
      return RoundStatus.REGISTER_OPEN;
    }
    // 접수 마감~시험 전 구간 (regEnd < today < examDate)
    if (regEnd != null && examDate != null && today.isAfter(regEnd) && today.isBefore(examDate)) {
      return RoundStatus.BEFORE_EXAM;
    }
    // 안전망: 정보 부족/엣지 케이스
    // examDate만 있고 오늘보다 미래면 시험 전
    if (examDate != null && today.isBefore(examDate)) {
      return RoundStatus.BEFORE_EXAM;
    }
    return RoundStatus.BEFORE_REG;
  }
}
