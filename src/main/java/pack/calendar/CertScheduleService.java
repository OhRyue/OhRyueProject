package pack.calendar;

import org.springframework.stereotype.Service;
import pack.calendar.dto.CertScheduleDto;

import java.util.List;

@Service
public class CertScheduleService {

  private final CertScheduleRepository repo;

  public CertScheduleService(CertScheduleRepository repo) {
    this.repo = repo;
  }

  public List<CertScheduleDto> listByCertAndYear(Long certId, Integer year) {
    List<CertSchedule> list = (year != null)
        ? repo.findByCertIdAndYearOrderByExamDateAsc(certId, year)
        : repo.findByCertIdOrderByExamDateAsc(certId);
    return list.stream().map(CertScheduleDto::from).toList();
  }
}
