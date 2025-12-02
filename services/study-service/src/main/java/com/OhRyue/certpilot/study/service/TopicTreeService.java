package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class TopicTreeService {

    private final CurriculumGateway curriculumGateway;
    private final CertCurriculumClient certCurriculumClient;

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

    /**
     * rootTopicId의 직접 자식 토픽 ID들을 가져옵니다.
     * REVIEW 모드에서 사용됩니다 (2레벨 토픽의 자식인 3레벨 토픽들만 필요).
     * 
     * @param rootTopicId 부모 토픽 ID
     * @param examMode 필터링할 시험 모드 (WRITTEN/PRACTICAL), null이면 필터링 안 함
     * @return 자식 토픽 ID 집합
     */
    public Set<Long> childrenOf(Long rootTopicId, String examMode) {
        if (rootTopicId == null) {
            return Collections.emptySet();
        }

        try {
            log.debug("Requesting children for topicId={}, examMode={}", rootTopicId, examMode);
            CertCurriculumClient.TopicListResponse response = certCurriculumClient.listTopics(null, examMode, rootTopicId);
            
            if (response == null) {
                log.warn("Null response from cert-service for topicId={}, examMode={}", rootTopicId, examMode);
                return Collections.emptySet();
            }
            
            if (response.topics() == null) {
                log.warn("Null topics list in response for topicId={}, examMode={}", rootTopicId, examMode);
                return Collections.emptySet();
            }
            
            if (response.topics().isEmpty()) {
                log.warn("No children found for topicId={}, examMode={}, fallback=empty set", rootTopicId, examMode);
                return Collections.emptySet();
            }
            
            Set<Long> childIds = response.topics().stream()
                    .filter(topic -> topic != null && topic.id() != null)  // null 체크
                    .map(CertCurriculumClient.TopicResponse::id)
                    .collect(Collectors.toSet());
            
            log.info("Found {} children for topicId={}, examMode={}: {}", childIds.size(), rootTopicId, examMode, childIds);
            
            if (childIds.isEmpty()) {
                log.warn("After filtering, no valid child IDs for topicId={}, examMode={}", rootTopicId, examMode);
                return Collections.emptySet();
            }
            
            return childIds;
        } catch (Exception e) {
            log.error("Failed to get children from cert-service. rootId={}, examMode={}, cause={}",
                    rootTopicId, examMode, e.getMessage(), e);
            // 예외 발생 시 빈 집합 반환 (fallback은 호출자에서 처리)
            return Collections.emptySet();
        }
    }
    
    /**
     * rootTopicId의 직접 자식 토픽 ID들을 가져옵니다 (examMode 필터링 없음).
     * 기존 코드 호환성을 위한 오버로드.
     */
    public Set<Long> childrenOf(Long rootTopicId) {
        return childrenOf(rootTopicId, null);
    }

    // 기존 코드와의 호환용 alias
    public Set<Long> descendantIds(Long rootTopicId) {
        return descendantsOf(rootTopicId);
    }
}
