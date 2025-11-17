package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserXpLedger;
import com.OhRyue.certpilot.progress.domain.enums.XpReason;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;

public interface UserXpLedgerRepository extends JpaRepository<UserXpLedger, Long> {

  @Query("""
      SELECT COALESCE(SUM(l.delta),0)
      FROM UserXpLedger l
      WHERE l.userId = :userId
        AND l.createdAt >= :since
      """)
  long sumDeltaSince(@Param("userId") String userId, @Param("since") Instant since);

  boolean existsByUserIdAndReasonAndRefId(String userId, XpReason reason, String refId);

}
