package pack.calendar;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pack.calendar.dto.RoundWithCert;

import java.util.List;

public interface CertScheduleRepository extends JpaRepository<CertSchedule, Long> {

  // 기존 단순 메서드도 유지해도 되지만, 아래 조인 프로젝션을 메인으로 사용할 거예요.

  @Query("""
    select 
      s.id as id,
      s.certId as certId,
      s.year as year,
      s.term as term,
      s.regStart as regStart,
      s.regEnd as regEnd,
      s.examDate as examDate,
      c.name as certName
    from CertSchedule s
      join pack.certs.Certificate c on c.id = s.certId
    where s.certId = :certId
      and (:year is null or s.year = :year)
    order by s.examDate asc
  """)
  List<RoundWithCert> findRoundsWithCertName(
      @Param("certId") Long certId,
      @Param("year") Integer year
  );
}
