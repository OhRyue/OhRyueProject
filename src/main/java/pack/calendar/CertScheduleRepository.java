package pack.calendar;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CertScheduleRepository extends JpaRepository<CertSchedule, Long> {

  List<CertSchedule> findByCertIdOrderByExamDateAsc(Long certId);

  List<CertSchedule> findByCertIdAndYearOrderByExamDateAsc(Long certId, Integer year);
}
