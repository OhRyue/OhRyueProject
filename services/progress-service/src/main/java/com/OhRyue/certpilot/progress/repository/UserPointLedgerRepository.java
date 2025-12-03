package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserPointLedger;
import com.OhRyue.certpilot.progress.domain.enums.PointReason;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface UserPointLedgerRepository extends JpaRepository<UserPointLedger, Long> {
  
  /**
   * 사용자가 구매한 아이템의 총액을 계산합니다.
   * PURCHASE 타입의 delta는 음수이므로 절댓값을 반환합니다.
   */
  @Query("""
      SELECT COALESCE(ABS(SUM(l.delta)), 0)
      FROM UserPointLedger l
      WHERE l.userId = :userId
        AND l.reason = :reason
      """)
  long sumPurchaseTotal(@Param("userId") String userId, @Param("reason") PointReason reason);
}
