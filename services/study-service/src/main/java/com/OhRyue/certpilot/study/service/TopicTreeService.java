package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CurriculumGateway;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Topic 트리 유틸 (MSA 버전)
 * - 실제 트리 계산/DB 조회는 cert-service가 담당
 * - study-service에서는 CurriculumGateway를 통해 후손 topic id만 받아서 사용
 */
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class TopicTreeService {

    private final CurriculumGateway curriculumGateway;

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
        try {
            Set<Long> ids = curriculumGateway.getDescendantTopicIds(rootTopicId);
            if (ids == null || ids.isEmpty()) {
                return Set.of(rootTopicId);
            }
            return new LinkedHashSet<>(ids);
        } catch (Exception e) {
            log.warn("Failed to load topic tree from cert-service. fallback=root only. cause={}", e.getMessage());
            return Set.of(rootTopicId);
        }
    }
}
