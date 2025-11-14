package com.OhRyue.certpilot.cert.repository;

import com.OhRyue.certpilot.cert.domain.CertEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface CertRepository extends JpaRepository<CertEntity, Long> {

    Optional<CertEntity> findByQnetJmCd(String qnetJmCd);

    /**
     * 온보딩에서 보여줄 "Top N" 자격증을 가져오기 위한 쿼리입니다.
     * - 현재는 단순히 id 오름차순 기준으로 상위 N개를 사용합니다.
     *   (나중에 우선순위 컬럼이 생기면 ORDER BY를 그걸로 바꾸면 됩니다.)
     */
    List<CertEntity> findTop4ByOrderByIdAsc();
}

