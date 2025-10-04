package pack.calendar;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import pack.calendar.dto.CertRoundDto;

// 👇 추가
import pack.calendar.dto.FullCalendarEventDto;
import java.util.Arrays;

import java.util.List;

@Tag(name = "Calendar", description = "자격증 회차/개인 캘린더 API")
@CrossOrigin(origins = "http://localhost:5173", allowCredentials = "true")
@Validated
@RestController
@RequiredArgsConstructor
@RequestMapping("/calendar")
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

  // 👇 Step E: FullCalendar 연동용 임시 피드(더미)
  @Operation(summary = "내 캘린더(스텁)", description = "FullCalendar가 바로 쓸 수 있는 임시 이벤트 배열을 반환합니다.")
  @GetMapping("/me")
  public List<FullCalendarEventDto> myCalendarStub() {
    return Arrays.asList(
        new FullCalendarEventDto("r-2025-1", "정보처리기사 1회 필기", "2025-02-10"),
        new FullCalendarEventDto("r-2025-2", "정보처리기사 2회 필기", "2025-06-03")
    );
  }
}
