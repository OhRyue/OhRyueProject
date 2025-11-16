package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.repository.TopicRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Topic 트리 유틸
 * - topic 테이블(parent_id 기반)을 사용해
 *   rootTopicId 자신 + 모든 하위 토픽 id 집합을 계산한다.
 *
 * 예)
 *  root = 11101  → {11101}
 *  root = 11001  → {11001, 11101, 11102, 11103}
 *  root = 10001  → {10001, 11001~, 111xx~, 112xx~, ...}
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class TopicTreeService {

  private final TopicRepository topicRepository;

  /**
   * 기존 시그니처 유지용 래핑
   */
  public Set<Long> descendantIds(Long rootId) {
    return descendantsOf(rootId);
  }

  /**
   * rootTopicId 포함 모든 후손 토픽 id 집합 반환
   */
  public Set<Long> descendantsOf(Long rootTopicId) {
    if (rootTopicId == null) {
      return Set.of();
    }

    try {
      // 전체 토픽을 한 번에 읽어서 parent_id → children 맵 구성
      List<Topic> all = topicRepository.findAll();
      if (all.isEmpty()) {
        log.warn("Topic table is empty. fallback to root only. rootId={}", rootTopicId);
        return Set.of(rootTopicId);
      }

      Map<Long, List<Long>> childrenMap = all.stream()
          .filter(t -> t.getParentId() != null)
          .collect(Collectors.groupingBy(
              Topic::getParentId,
              Collectors.mapping(Topic::getId, Collectors.toList())
          ));

      Set<Long> visited = new LinkedHashSet<>();
      Deque<Long> stack = new ArrayDeque<>();

      // 루트 포함해서 내려가기
      visited.add(rootTopicId);
      stack.push(rootTopicId);

      while (!stack.isEmpty()) {
        Long current = stack.pop();
        List<Long> children = childrenMap.getOrDefault(current, List.of());
        for (Long childId : children) {
          if (visited.add(childId)) {
            stack.push(childId);
          }
        }
      }

      return visited;
    } catch (Exception e) {
      log.warn("Failed to build topic tree from local DB. fallback=root only. cause={}", e.getMessage());
      return Set.of(rootTopicId);
    }
  }
}
