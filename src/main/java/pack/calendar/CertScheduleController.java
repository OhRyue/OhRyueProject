package pack.calendar;

import jakarta.validation.constraints.Min;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import pack.calendar.dto.CertScheduleDto;

import java.util.List;

/**
 * URL 설계:
 * - GET /certs/{id}/schedules?year=2025   : 해당 자격증의 2025년 회차별 일정
 * - GET /certs/{id}/schedules             : 전체 년도(과거~현재) 회차별 일정
 */
@RestController
@RequestMapping(path = "/certs/{certId}/schedules", produces = MediaType.APPLICATION_JSON_VALUE)
public class CertScheduleController {

  private final CertScheduleService service;

  public CertScheduleController(CertScheduleService service) {
    this.service = service;
  }

  @GetMapping
  public List<CertScheduleDto> list(
      @PathVariable Long certId,
      @RequestParam(required = false) @Min(2000) Integer year
  ) {
    return service.listByCertAndYear(certId, year);
  }
}
