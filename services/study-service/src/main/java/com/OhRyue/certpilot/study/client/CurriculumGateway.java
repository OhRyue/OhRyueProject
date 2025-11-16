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

  /**
   * cert-service 로부터 topic + concept 를 한 번에 조회하는 헬퍼.
   * - 서킷브레이커 이름: certCurriculum
   * - 실패 시 fallback 메서드로 위임
   */
  @CircuitBreaker(name = "certCurriculum", fallbackMethod = "getConceptWithTopicFallback")
  public CurriculumConcept getConceptWithTopic(Long topicId) {

    CertCurriculumClient.TopicResponse topic = certCurriculumClient.getTopic(topicId);
    CertCurriculumClient.ConceptResponse concept = certCurriculumClient.getConcept(topicId);

    return new CurriculumConcept(
        topicId,
        topic.title(),
        concept.sectionsJson()
    );
  }

  /**
   * fallbackMethod 시그니처 규칙:
   * - 원본 메서드 파라미터 + 마지막에 Throwable
   */
  public CurriculumConcept getConceptWithTopicFallback(Long topicId, Throwable t) {
    log.warn("Failed to load concept from cert-service. topicId={}, cause={}", topicId, t.toString());
    throw new ExternalCurriculumException(
        "cert-service 커리큘럼 정보를 불러오지 못했습니다. topicId=" + topicId,
        t
    );
  }

  /**
   * rootTopicId 기준으로 모든 후손 토픽 id 리스트를 cert-service 에서 조회.
   * - cert-service 내부 엔드포인트: /api/certs/internal/curriculum/topics/{rootTopicId}/descendants
   * - 실패 시 fallback 에서 root 만 포함한 Set 반환
   */
  @CircuitBreaker(name = "certCurriculum", fallbackMethod = "getDescendantTopicIdsFallback")
  public Set<Long> getDescendantTopicIds(Long rootTopicId) {
    List<Long> ids = certCurriculumClient.getDescendantTopicIds(rootTopicId);
    if (ids == null || ids.isEmpty()) {
      return Set.of(rootTopicId);
    }
    // 순서 보존 + 중복 제거
    return new LinkedHashSet<>(ids);
  }

  public Set<Long> getDescendantTopicIdsFallback(Long rootTopicId, Throwable t) {
    log.warn("Failed to load descendant topic ids from cert-service. rootTopicId={}, cause={}",
        rootTopicId, t.toString());
    // 완전 장애 시에도 최소한 루트 토픽만 사용하도록 degrade
    return Set.of(rootTopicId);
  }

  /**
   * Gateway 내부용 DTO
   */
  public record CurriculumConcept(
      Long topicId,
      String topicTitle,
      String sectionsJson
  ) {}

  /**
   * cert-service 장애를 표현하는 커스텀 런타임 예외
   */
  public static class ExternalCurriculumException extends RuntimeException {
    public ExternalCurriculumException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
