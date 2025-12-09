package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.TagMaster;
import com.OhRyue.certpilot.study.domain.enums.TagDomain;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Collection;
import java.util.List;

public interface TagMasterRepository extends JpaRepository<TagMaster, String> {

  /**
   * 도메인별 태그 목록 조회 (order_no ASC, code ASC)
   */
  List<TagMaster> findByDomainOrderByOrderNoAscCodeAsc(TagDomain domain);

  /**
   * 전체 태그 목록 조회 (domain, order_no ASC, code ASC)
   */
  @Query("select t from TagMaster t order by t.domain, t.orderNo asc, t.code asc")
  List<TagMaster> findAllOrdered();

  /**
   * 태그 코드 목록으로 태그 마스터 조회
   */
  List<TagMaster> findByCodeIn(Collection<String> codes);

  /**
   * 도메인과 태그 코드 목록으로 조회
   */
  List<TagMaster> findByDomainAndCodeIn(TagDomain domain, Collection<String> codes);
}

