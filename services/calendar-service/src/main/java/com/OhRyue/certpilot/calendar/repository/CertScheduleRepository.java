package com.OhRyue.certpilot.calendar.repository;

import com.OhRyue.certpilot.calendar.entity.CertSchedule;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface CertScheduleRepository extends JpaRepository<CertSchedule, Long> {

  @Query("""
    select
      s.id as id,
      s.certId as certId,
      s.year as year,
      s.term as term,
      s.regStart as regStart,
      s.regEnd as regEnd,
      s.examDate as examDate
    from CertSchedule s
    where s.certId = :certId
      and (:year is null or s.year = :year)
    order by s.examDate asc
  """)
  List<RoundProjection> findRounds(
      @Param("certId") Long certId,
      @Param("year") Integer year
  );
}

