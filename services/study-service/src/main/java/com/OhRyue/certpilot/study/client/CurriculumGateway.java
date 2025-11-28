package com.OhRyue.certpilot.study.client;

import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class CurriculumGateway {

    private final CertCurriculumClient certCurriculumClient;

    @CircuitBreaker(name = "certCurriculum", fallbackMethod = "getConceptWithTopicFallback")
    public CurriculumConcept getConceptWithTopic(Long topicId) {
        CertCurriculumClient.TopicResponse topic = certCurriculumClient.getTopic(topicId);
        CertCurriculumClient.ConceptResponse concept = certCurriculumClient.getConcept(topicId);

        // null 체크: cert-service가 null을 반환할 수 있음
        if (topic == null) {
            throw new RuntimeException("Topic 정보를 찾을 수 없습니다. topicId=" + topicId);
        }
        if (concept == null) {
            throw new RuntimeException("Concept 정보를 찾을 수 없습니다. topicId=" + topicId);
        }

        return new CurriculumConcept(
                topicId,
                topic.title() != null ? topic.title() : "",
                concept.sectionsJson() != null ? concept.sectionsJson() : "{}"
        );
    }

    public CurriculumConcept getConceptWithTopicFallback(Long topicId, Throwable t) {
        log.warn("Failed to load concept from cert-service. topicId={}, cause={}", topicId, t.toString());
        throw new ExternalCurriculumException(
                "cert-service 커리큘럼 정보를 불러오지 못했습니다. topicId=" + topicId,
                t
        );
    }

    @CircuitBreaker(name = "certCurriculum", fallbackMethod = "getDescendantTopicIdsFallback")
    public Set<Long> getDescendantTopicIds(Long rootTopicId) {
        CertCurriculumClient.DescendantsResponse resp =
                certCurriculumClient.getDescendantTopicIds(rootTopicId);

        List<Long> ids = (resp != null ? resp.ids() : null);
        if (ids == null || ids.isEmpty()) {
            return Set.of(rootTopicId);
        }
        return new LinkedHashSet<>(ids);
    }

    public Set<Long> getDescendantTopicIdsFallback(Long rootTopicId, Throwable t) {
        log.warn("Failed to load descendant topic ids from cert-service. rootTopicId={}, cause={}",
                rootTopicId, t.toString());
        return Set.of(rootTopicId);
    }

    public record CurriculumConcept(
            Long topicId,
            String topicTitle,
            String sectionsJson
    ) {}

    public static class ExternalCurriculumException extends RuntimeException {
        public ExternalCurriculumException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}
