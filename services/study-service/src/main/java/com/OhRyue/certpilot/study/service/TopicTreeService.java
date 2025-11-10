package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.repository.TopicRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigInteger;
import java.util.*;

/**
 * Topic 트리 유틸:
 * - MySQL 8.0의 재귀 CTE로 rootTopicId 포함 모든 후손 토픽 id를 조회
 * - CTE 실패 시(로컬 설정/권한 문제 등) 엔티티 매니저로 children을 반복 조회하는 폴백을 사용
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class TopicTreeService {

  private final TopicRepository repo;

  @PersistenceContext
  // ✅ JPA 필드 주입은 final 불가 → final 제거
  private EntityManager em;

  /**
   * 기존 메서드명/시그니처 유지
   */
  public Set<Long> descendantIds(Long rootId) {
    return descendantsOf(rootId);
  }

  /**
   * 신규 오버로드: 명시적 이름의 동등 기능
   */
  public Set<Long> descendantsOf(Long rootTopicId) {
    // 1) MySQL 8.0 재귀 CTE 시도
    try {
      String sql = """
          WITH RECURSIVE t AS (
            SELECT id, parentId FROM topic WHERE id = :root
            UNION ALL
            SELECT c.id, c.parentId
            FROM topic c
            INNER JOIN t ON c.parentId = t.id
          )
          SELECT id FROM t
          """;
      Query q = em.createNativeQuery(sql);
      q.setParameter("root", rootTopicId);
      @SuppressWarnings("unchecked")
      List<BigInteger> rows = q.getResultList();

      Set<Long> out = new LinkedHashSet<>();
      for (BigInteger bi : rows) out.add(bi.longValue());
      // 방어: 최소한 루트는 포함
      if (out.isEmpty()) out.add(rootTopicId);
      return out;
    } catch (Exception e) {
      log.warn("Recursive CTE failed, fallback to iterative select. cause={}", e.getMessage());
      // 2) 폴백: iterative children select (findAll 스캔 제거)
      return fetchDescendantsIteratively(rootTopicId);
    }
  }

  /**
   * 폴백: parentId 인덱스를 활용해 반복적으로 자식만 조회
   */
  private Set<Long> fetchDescendantsIteratively(Long rootId) {
    Set<Long> visited = new LinkedHashSet<>();
    Deque<Long> dq = new ArrayDeque<>();
    dq.add(rootId);

    while (!dq.isEmpty()) {
      Long cur = dq.poll();
      if (!visited.add(cur)) continue;

      // children ids만 조회 (전체 findAll() X)
      String sql = "SELECT id FROM topic WHERE parentId = :pid";
      Query q = em.createNativeQuery(sql);
      q.setParameter("pid", cur);
      @SuppressWarnings("unchecked")
      List<BigInteger> childIds = q.getResultList();

      for (BigInteger bi : childIds) dq.add(bi.longValue());
    }
    return visited;
  }
}
