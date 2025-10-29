package com.OhRyue.certpilot.calendar.web;

import com.OhRyue.certpilot.calendar.dto.CertRoundDto;
import com.OhRyue.certpilot.calendar.dto.FullCalendarEventDto;
import com.OhRyue.certpilot.calendar.service.CalendarService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Calendar", description = "자격증 회차/개인 캘린더 API")
@Validated
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/calendar")
public class CalendarController {

  private final CalendarService calendarService;

  @Operation(summary = "회차별 일정 조회", description = "certId(필수), year(선택)로 회차별 일정과 상태를 조회합니다.")
  @GetMapping("/rounds")
  public List<CertRoundDto> getRounds(
      @Parameter(description = "자격증 ID", required = true)
      @RequestParam Long certId,

      @Parameter(description = "연도 (e.g. 2025)")
      @RequestParam(required = false) @Min(2000) Integer year
  ) {
    return calendarService.getRounds(certId, year);
  }

  @Operation(summary = "내 캘린더 피드(FullCalendar)", description = "title에 자격증명 포함 (예: 정보처리기사 2025년 1회)")
  @GetMapping("/me")
  public List<FullCalendarEventDto> myCalendar(
      @RequestParam Long certId,
      @RequestParam(required = false) @Min(2000) Integer year
  ) {
    return calendarService.getMyEvents(certId, year);
  }
}

