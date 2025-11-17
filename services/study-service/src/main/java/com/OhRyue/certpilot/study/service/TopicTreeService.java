package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CurriculumGateway;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class TopicTreeService {

    private final CurriculumGateway curriculumGateway;

    /**
     * cert-service 의 INTERNAL 트리 API를 이용해서
     * rootTopicId 포함 모든 후손 토픽 id 집합을 가져옵니다.
     */
    public Set<Long> descendantsOf(Long rootTopicId) {
        if (rootTopicId == null) {
            return Collections.emptySet();
        }

        try {
            Set<Long> ids = curriculumGateway.getDescendantTopicIds(rootTopicId);
            if (ids == null || ids.isEmpty()) {
                return Set.of(rootTopicId);
            }
            return ids;
        } catch (Exception e) {
            log.warn("Failed to build topic tree from cert-service. fallback=root only. rootId={}, cause={}",
                    rootTopicId, e.toString());
            return Set.of(rootTopicId);
        }
    }

    // 기존 코드와의 호환용 alias
    public Set<Long> descendantIds(Long rootTopicId) {
        return descendantsOf(rootTopicId);
    }
}
