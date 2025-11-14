package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.repository.TopicRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class TopicTreeService {

    private final TopicRepository topicRepository;

    /**
     * rootTopicId 포함, 모든 후손 토픽 id 반환
     * - parent_id 인덱스를 이용한 반복 조회 방식
     */
    public List<Long> descendantsOf(Long rootTopicId) {
        Set<Long> visited = new LinkedHashSet<>();
        Deque<Long> dq = new ArrayDeque<>();
        dq.add(rootTopicId);

        while (!dq.isEmpty()) {
            Long cur = dq.poll();
            if (!visited.add(cur)) {
                continue;
            }

            List<Topic> children = topicRepository.findByParentId(cur);
            for (Topic child : children) {
                dq.add(child.getId());
            }
        }

        if (visited.isEmpty()) {
            visited.add(rootTopicId);
        }
        return List.copyOf(visited);
    }
}
