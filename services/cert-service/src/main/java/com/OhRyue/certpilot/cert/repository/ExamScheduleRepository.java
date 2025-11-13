package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ExamScheduleRepository extends JpaRepository<ExamScheduleEntity, Long> {

  @Query("""
      select e from ExamScheduleEntity e
      where (:implYy is null or e.implYy = :implYy)
        and (:qualgbCd is null or e.qualgbCd = :qualgbCd)
        and (:jmCd is null or e.jmCd = :jmCd)
      order by e.implYy desc, e.implSeq desc
      """)
  List<ExamScheduleEntity> search(@Param("implYy") String implYy,
                                  @Param("qualgbCd") String qualgbCd,
                                  @Param("jmCd") String jmCd);

  ExamScheduleEntity findFirstBySourceAndImplYyAndImplSeqAndJmCd(String source,
                                                                 String implYy,
                                                                 String implSeq,
                                                                 String jmCd);

  List<ExamScheduleEntity> findTop5BySourceAndJmCdOrderByImplYyDescImplSeqDesc(String source, String jmCd);
}

