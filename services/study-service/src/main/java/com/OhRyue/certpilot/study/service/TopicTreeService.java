package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.client.CertCurriculumClient;
import com.OhRyue.certpilot.study.client.CurriculumGateway;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

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
        return childrenOf(rootTopicId, examMode, null);
    }
    
    /**
     * rootTopicId의 직접 자식 토픽 ID들을 가져옵니다.
     * 
     * @param rootTopicId 부모 토픽 ID
     * @param examMode 필터링할 시험 모드 (WRITTEN/PRACTICAL), null이면 필터링 안 함
     * @param certId 자격증 ID (선택사항, 있으면 더 정확한 조회 가능)
     * @return 자식 토픽 ID 집합
     */
    public Set<Long> childrenOf(Long rootTopicId, String examMode, Long certId) {
        if (rootTopicId == null) {
            return Collections.emptySet();
        }

        // ========== [study] TopicTree request 로깅 ==========
        log.info("[study] TopicTree request: certId={}, rootTopicId={}, examMode={}", certId, rootTopicId, examMode);
        log.info("[study] TopicTree API: GET /api/cert/topics?certId={}&mode={}&parentId={}", certId, examMode, rootTopicId);
        
        // Retry 로직: Eureka 인스턴스 발견 지연 대응
        int maxRetries = 3;
        long retryDelayMs = 500; // 500ms, 1s, 1.5s
        
        for (int attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                if (attempt > 1) {
                    long delay = retryDelayMs * attempt;
                    log.info("[study] TopicTree retry attempt {}/{}: waiting {}ms before retry", attempt, maxRetries, delay);
                    try {
                        Thread.sleep(delay);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Retry interrupted", ie);
                    }
                }
                
                CertCurriculumClient.TopicListResponse response = certCurriculumClient.listTopics(certId, examMode, rootTopicId);
            
                // ========== 호출 성공, 응답 파싱 단계 ==========
                if (response == null) {
                    log.error("[study] TopicTree FAILED: response is null, rootTopicId={}, examMode={}, certId={}", 
                            rootTopicId, examMode, certId);
                    log.error("[study] TopicTree fallback used: reason=null_response, returning empty set");
                    return Collections.emptySet();
                }
                
                if (response.topics() == null) {
                    log.error("[study] TopicTree FAILED: response.topics() is null, rootTopicId={}, examMode={}, certId={}", 
                            rootTopicId, examMode, certId);
                    log.error("[study] TopicTree fallback used: reason=null_topics_field, returning empty set");
                    return Collections.emptySet();
                }
                
                // ========== 응답 성공, 데이터 확인 ==========
                int responseSize = response.topics().size();
                log.info("[study] TopicTree response: topics count={}, rootTopicId={}, examMode={}, certId={}", 
                        responseSize, rootTopicId, examMode, certId);
                
                if (responseSize == 0) {
                    log.warn("[study] TopicTree response: empty list (size=0), rootTopicId={}, examMode={}, certId={}", 
                            rootTopicId, examMode, certId);
                    log.warn("[study] TopicTree 원인 분석: cert-service가 빈 리스트를 반환했습니다.");
                    log.warn("[study] TopicTree 가능한 원인: 1) DB에 실제로 자식 토픽이 없음, 2) examMode 필터로 제외됨, 3) certId 불일치, 4) cert-service 쿼리 버그");
                    log.warn("[study] TopicTree fallback used: reason=empty_response_list, returning empty set");
                    return Collections.emptySet();
                }
                
                // ========== 파싱 및 필터링 ==========
                List<Long> rawChildIds = response.topics().stream()
                        .filter(topic -> topic != null && topic.id() != null)
                        .map(CertCurriculumClient.TopicResponse::id)
                        .collect(Collectors.toList());
                
                Set<Long> childIds = new LinkedHashSet<>(rawChildIds);
                
                log.info("[study] TopicTree response: descendantTopicIds={} (size={}), rootTopicId={}, examMode={}, certId={}", 
                        childIds, childIds.size(), rootTopicId, examMode, certId);
                
                if (childIds.isEmpty()) {
                    log.warn("[study] TopicTree FAILED: 파싱 후 유효한 ID가 없음, rootTopicId={}, examMode={}, certId={}", 
                            rootTopicId, examMode, certId);
                    log.warn("[study] TopicTree 원인 분석: 응답 topics 리스트는 있었지만(null이 아님), 모든 항목의 id가 null이었습니다.");
                    log.warn("[study] TopicTree fallback used: reason=all_ids_null_after_parsing, returning empty set");
                    return Collections.emptySet();
                }
                
                // ========== 성공 ==========
                log.info("[study] TopicTree SUCCESS: descendantTopicIds={} (size={}), attempt={}", childIds, childIds.size(), attempt);
                return childIds;
                
            } catch (feign.FeignException e) {
                // ========== Feign 호출 실패 (HTTP 에러) ==========
                int status = e.status();
                String responseBody = e.contentUTF8();
                log.warn("[study] TopicTree FAILED (attempt {}/{}): FeignException status={}, rootTopicId={}, examMode={}, certId={}", 
                        attempt, maxRetries, status, rootTopicId, examMode, certId);
                log.warn("[study] TopicTree FAILED: responseBody={}", responseBody);
                log.warn("[study] TopicTree FAILED: exception class={}, message={}", 
                        e.getClass().getName(), e.getMessage());
                
                // 401 Unauthorized - 인증 실패 (재시도 불필요, 즉시 실패)
                if (status == 401) {
                    log.error("[study] TopicTree FAILED: 401 Unauthorized - Internal JWT 인증 실패");
                    log.error("[study] TopicTree 원인 분석:");
                    log.error("[study]   1. INTERNAL_JWT_SECRET이 study-service와 cert-service 간 불일치");
                    log.error("[study]   2. issuer/audience 불일치");
                    log.error("[study]   3. 토큰 만료 또는 형식 오류");
                    log.error("[study] TopicTree 해결 방법: cert-service 로그에서 'Internal JWT' 관련 에러 확인");
                    throw new ResponseStatusException(
                        HttpStatus.BAD_GATEWAY,
                        String.format("cert-service authentication failed (401). rootTopicId=%d, examMode=%s. " +
                                "Check INTERNAL_JWT_SECRET consistency between study-service and cert-service.",
                                rootTopicId, examMode),
                        e);
                }
                
                // 503 Service Unavailable (Eureka 인스턴스 없음) - 재시도
                if (status == 503) {
                    log.warn("[study] TopicTree 원인: cert-service 인스턴스를 Eureka에서 찾지 못함 (No servers available)");
                    if (attempt < maxRetries) {
                        log.info("[study] TopicTree: 재시도 예정 (attempt {}/{})", attempt + 1, maxRetries);
                        continue; // 재시도
                    } else {
                        log.error("[study] TopicTree FAILED: 모든 재시도 실패 ({}회), status=503", maxRetries);
                        throw new ResponseStatusException(
                            HttpStatus.SERVICE_UNAVAILABLE,
                            String.format("cert-service is unavailable (Eureka instance not found). rootTopicId=%d, examMode=%s, attempts=%d",
                                    rootTopicId, examMode, maxRetries),
                            e);
                    }
                }
                
                // 502 Bad Gateway - 재시도
                if (status == 502) {
                    if (attempt < maxRetries) {
                        log.info("[study] TopicTree: 재시도 예정 (attempt {}/{})", attempt + 1, maxRetries);
                        continue; // 재시도
                    } else {
                        log.error("[study] TopicTree FAILED: 모든 재시도 실패 ({}회), status=502", maxRetries);
                        throw new ResponseStatusException(
                            HttpStatus.BAD_GATEWAY,
                            String.format("cert-service returned Bad Gateway. rootTopicId=%d, examMode=%s, attempts=%d",
                                    rootTopicId, examMode, maxRetries),
                            e);
                    }
                }
                
                // 404는 데이터 없음으로 간주 (재시도 불필요)
                if (status == 404) {
                    log.warn("[study] TopicTree: 404 Not Found - 데이터 없음으로 간주, empty set 반환");
                    return Collections.emptySet();
                }
                
                // 기타 HTTP 에러는 empty set 반환 (기존 동작 유지)
                log.error("[study] TopicTree fallback used: reason=feign_exception_status_{}, returning empty set", status);
                return Collections.emptySet();
                
            } catch (Exception e) {
                // ========== 기타 예외 ==========
                log.error("[study] TopicTree FAILED (attempt {}/{}): unexpected exception, rootTopicId={}, examMode={}, certId={}", 
                        attempt, maxRetries, rootTopicId, examMode, certId);
                log.error("[study] TopicTree FAILED: exception class={}, message={}", 
                        e.getClass().getName(), e.getMessage());
                
                if (attempt < maxRetries) {
                    log.info("[study] TopicTree: 재시도 예정 (attempt {}/{})", attempt + 1, maxRetries);
                    continue; // 재시도
                } else {
                    log.error("[study] TopicTree FAILED: 모든 재시도 실패 ({}회)", maxRetries);
                    throw new ResponseStatusException(
                        HttpStatus.INTERNAL_SERVER_ERROR,
                        String.format("cert-service call failed. rootTopicId=%d, examMode=%s, attempts=%d",
                                rootTopicId, examMode, maxRetries),
                        e);
                }
            }
        }
        
        // 모든 재시도 실패 (이 코드는 도달하지 않아야 함)
        log.error("[study] TopicTree FAILED: 모든 재시도 실패, empty set 반환");
        return Collections.emptySet();
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
