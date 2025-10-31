package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.Topic;
import com.OhRyue.certpilot.study.repository.TopicRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TopicTreeService {

  private final TopicRepository repo;

  public Set<Long> descendantIds(Long rootId) {
    Set<Long> out = new LinkedHashSet<>();
    Deque<Long> dq = new ArrayDeque<>();
    dq.add(rootId);
    while (!dq.isEmpty()) {
      Long cur = dq.poll();
      out.add(cur);
      List<Topic> children = repo.findAll().stream()
          .filter(t -> Objects.equals(t.getParentId(), cur))
          .toList();
      for (Topic c : children) dq.add(c.getId());
    }
    return out;
  }
}
