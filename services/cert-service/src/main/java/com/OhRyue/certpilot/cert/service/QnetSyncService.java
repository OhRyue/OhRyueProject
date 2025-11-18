package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.client.DataFeignClient;
import com.OhRyue.certpilot.cert.client.QnetFeignClient;
import com.OhRyue.certpilot.cert.config.QnetProperties;
import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.dto.external.ExamScheduleItem;
import com.OhRyue.certpilot.cert.dto.external.OpenQuestionItem;
import com.OhRyue.certpilot.cert.dto.external.QnetApiResponse;
import com.OhRyue.certpilot.cert.dto.external.QualificationItem;
import com.OhRyue.certpilot.cert.repository.ExamScheduleRepository;
import com.OhRyue.certpilot.cert.repository.OpenQuestionRepository;
import com.OhRyue.certpilot.cert.repository.QualificationRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.function.Function;

@Service
@RequiredArgsConstructor
@Slf4j
public class QnetSyncService {

    private static final int DEFAULT_PAGE_SIZE = 100;

    private final QnetFeignClient qnetFeignClient;
    private final DataFeignClient dataGoFeignClient;
    private final QnetProperties properties;
    private final QualificationRepository qualificationRepository;
    private final ExamScheduleRepository examScheduleRepository;
    private final OpenQuestionRepository openQuestionRepository;
    private final QnetMapper mapper;
    private final ObjectMapper objectMapper;
    private final XmlMapper xmlMapper;

    /**
     * 자격(종목) 기본 정보 동기화
     * - Q-Net(openapi.q-net.or.kr) InquiryQualInfo/getList 사용
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackSync")
    @CacheEvict(value = {"cert-current", "cert-tips"}, allEntries = true)
    public SyncResult syncQualifications() {
        return fetchPaged(
                "qualification",
                params -> qnetFeignClient.getQualificationInfo(params),
                QualificationItem.class,
                item -> {
                    if (!StringUtils.hasText(item.getJmCd())) {
                        return SyncAction.SKIPPED;
                    }
                    QualificationEntity existing = qualificationRepository.findById(item.getJmCd()).orElse(null);
                    QualificationEntity entity = mapper.toQualification(item, existing);
                    qualificationRepository.save(entity);
                    return existing == null ? SyncAction.INSERTED : SyncAction.UPDATED;
                },
                extraParams -> {
                    // Q-Net JSON 응답을 받기 위한 파라미터
                    extraParams.put("_type", "json");
                }
        );
    }

    /**
     * 국가자격 시험일정 동기화
     * - data.go.kr(B490007) /qualExamSchd/getQualExamSchdList 사용
     * - jmCds 가 여러 개면 내부에서 순차 호출하여 aggregate
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackSync")
    @CacheEvict(value = {"cert-current", "cert-tips"}, allEntries = true)
    public SyncResult syncExamSchedules(Set<String> jmCds, String qualgbCd, String implYy) {
        if (jmCds != null && jmCds.size() > 1) {
            List<SyncResult> results = new ArrayList<>();
            for (String jm : jmCds) {
                results.add(syncExamSchedules(Set.of(jm), qualgbCd, implYy));
            }
            return SyncResult.aggregate("exam_schedule", results);
        }

        return fetchPaged(
                "exam_schedule",
                params -> dataGoFeignClient.getExamSchedules(params),
                ExamScheduleItem.class,
                item -> {
                    if (!StringUtils.hasText(item.getJmCd())) {
                        return SyncAction.SKIPPED;
                    }
                    ExamScheduleEntity existing =
                            examScheduleRepository.findFirstBySourceAndImplYyAndImplSeqAndJmCd(
                                    "QNET", item.getImplYy(), item.getImplSeq(), item.getJmCd());
                    ExamScheduleEntity entity = mapper.toExamSchedule("QNET", item, existing);
                    examScheduleRepository.save(entity);
                    return existing == null ? SyncAction.INSERTED : SyncAction.UPDATED;
                },
                extraParams -> {
                    // data.go.kr 쪽 JSON 포맷
                    extraParams.put("dataFormat", "json");

                    // 국가기술자격 기본값 (spec 기준) – 필요 시 다른 코드도 사용 가능
                    if (StringUtils.hasText(qualgbCd)) {
                        extraParams.put("qualgbCd", qualgbCd);
                    } else {
                        extraParams.put("qualgbCd", "T"); // 국가기술자격 기본값 (확실하지 않음 → 필요 시 실제 스펙에 맞게 수정)
                    }

                    if (jmCds != null && !jmCds.isEmpty()) {
                        extraParams.put("jmCd", jmCds.iterator().next());
                    }
                    if (StringUtils.hasText(implYy)) {
                        extraParams.put("implYy", implYy);
                    }
                }
        );
    }

    /**
     * 공개 문제 동기화
     * - data.go.kr(B490007) /openQst/getOpenQstList 사용
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackSync")
    @CacheEvict(value = {"cert-current", "cert-tips"}, allEntries = true)
    public SyncResult syncOpenQuestions(String jmCd) {
        return fetchPaged(
                "open_question",
                params -> dataGoFeignClient.getOpenQuestions(params),
                OpenQuestionItem.class,
                item -> {
                    if (item.getArtlSeq() == null) {
                        return SyncAction.SKIPPED;
                    }
                    OpenQuestionEntity existing = openQuestionRepository.findById(item.getArtlSeq()).orElse(null);
                    OpenQuestionEntity entity = mapper.toOpenQuestion(item, existing);
                    openQuestionRepository.save(entity);
                    return existing == null ? SyncAction.INSERTED : SyncAction.UPDATED;
                },
                extraParams -> {
                    // data.go.kr JSON 포맷
                    extraParams.put("dataFormat", "json");

                    // 국가기술자격 기본값 (공개문제는 대개 기술자격 기준 – 실제 스펙에 따라 조정 가능)
                    extraParams.put("qualgbCd", "T"); // 확실하지 않음 → 필요 시 포털 예제와 맞춰 조정

                    if (StringUtils.hasText(jmCd)) {
                        extraParams.put("jmCd", jmCd);
                    }
                }
        );
    }

    /* ==================== Resilience4j Fallback ==================== */

    public SyncResult fallbackSync(Throwable throwable) {
        log.warn("Q-Net sync fallback triggered: {}", throwable.getMessage());
        return SyncResult.failed(throwable.getMessage());
    }

    public SyncResult fallbackSync(Set<String> jmCds, String qualgbCd, String implYy, Throwable throwable) {
        return fallbackSync(throwable);
    }

    public SyncResult fallbackSync(String jmCd, Throwable throwable) {
        return fallbackSync(throwable);
    }

    /* ==================== 공통 페이징 호출 로직 ==================== */

    private <T> SyncResult fetchPaged(String name,
                                      Function<Map<String, String>, String> caller,
                                      Class<T> itemClass,
                                      Function<T, SyncAction> handler) {
        return fetchPaged(name, caller, itemClass, handler, params -> {});
    }

    private <T> SyncResult fetchPaged(String name,
                                      Function<Map<String, String>, String> caller,
                                      Class<T> itemClass,
                                      Function<T, SyncAction> handler,
                                      java.util.function.Consumer<Map<String, String>> extraParamConsumer) {
        int pageNo = 1;
        int inserted = 0;
        int updated = 0;
        int skipped = 0;
        int totalCount = 0;

        while (true) {
            Map<String, String> params = baseParams();
            params.put("pageNo", String.valueOf(pageNo));
            params.put("numOfRows", String.valueOf(DEFAULT_PAGE_SIZE));

            extraParamConsumer.accept(params);

            String raw = caller.apply(params);
            Optional<QnetApiResponse> optional = parseResponse(raw);
            if (optional.isEmpty()) {
                log.warn("[{}] Unable to parse response for page {}", name, pageNo);
                break;
            }

            QnetApiResponse response = optional.get();
            if (response.getResponse() == null || response.getResponse().getHeader() == null) {
                log.warn("[{}] Missing header for page {}", name, pageNo);
                break;
            }

            String resultCode = response.getResponse().getHeader().getResultCode();
            if (!"00".equals(resultCode)) {
                log.warn("[{}] API returned non-success code {} ({}) on page {}",
                        name,
                        resultCode,
                        response.getResponse().getHeader().getResultMsg(),
                        pageNo);
                break;
            }

            QnetApiResponse.Body body = response.getResponse().getBody();
            if (body == null) {
                break;
            }

            totalCount = Optional.ofNullable(body.getTotalCount()).orElse(0);
            List<T> items = convertItems(body.getItems(), itemClass);
            if (items.isEmpty()) {
                break;
            }

            for (T item : items) {
                try {
                    SyncAction action = handler.apply(item);
                    switch (action) {
                        case INSERTED -> inserted++;
                        case UPDATED -> updated++;
                        case SKIPPED -> skipped++;
                    }
                } catch (Exception ex) {
                    skipped++;
                    log.warn("[{}] Failed to process item: {}", name, ex.getMessage());
                }
            }

            int processed = (pageNo * DEFAULT_PAGE_SIZE);
            if (processed >= totalCount) {
                break;
            }
            pageNo++;
        }

        return new SyncResult(name, inserted, updated, skipped, totalCount, false, null);
    }

    /**
     * 공통 기본 파라미터 – 인증키만 세팅
     * - 인코딩은 절대 하지 않고, 디코딩 키를 그대로 넣습니다.
     * - 일부 API 가 serviceKey / ServiceKey 를 섞어 쓰는 케이스를 대비해 둘 다 세팅합니다.
     */
    private Map<String, String> baseParams() {
        Map<String, String> params = new HashMap<>();
        String key = Optional.ofNullable(properties.getKey()).orElse("");

        if (StringUtils.hasText(key)) {
            params.put("serviceKey", key);
            params.put("ServiceKey", key);
        }

        return params;
    }

    private Optional<QnetApiResponse> parseResponse(String raw) {
        if (!StringUtils.hasText(raw)) {
            return Optional.empty();
        }
        try {
            // JSON 응답
            return Optional.of(objectMapper.readValue(raw, QnetApiResponse.class));
        } catch (JsonProcessingException e) {
            try {
                // XML 응답 (혹시 설정이 잘못될 경우)
                return Optional.of(xmlMapper.readValue(raw, QnetApiResponse.class));
            } catch (Exception xmlEx) {
                log.debug("Failed to parse response as XML: {}", xmlEx.getMessage());
                return Optional.empty();
            }
        }
    }

    private <T> List<T> convertItems(JsonNode itemsNode, Class<T> itemClass) {
        List<T> results = new ArrayList<>();
        if (itemsNode == null || itemsNode.isNull()) {
            return results;
        }

        JsonNode actualNode = itemsNode;
        if (itemsNode.has("item")) {
            actualNode = itemsNode.get("item");
        }
        if (actualNode == null || actualNode.isNull()) {
            return results;
        }

        if (actualNode.isArray()) {
            actualNode.forEach(node -> convertNode(node, itemClass).ifPresent(results::add));
        } else {
            convertNode(actualNode, itemClass).ifPresent(results::add);
        }
        return results;
    }

    private <T> Optional<T> convertNode(JsonNode node, Class<T> itemClass) {
        try {
            return Optional.of(objectMapper.convertValue(node, itemClass));
        } catch (IllegalArgumentException ex) {
            log.debug("Failed to convert node to {}: {}", itemClass.getSimpleName(), ex.getMessage());
            return Optional.empty();
        }
    }

    @CacheEvict(value = {"cert-current", "cert-tips"}, allEntries = true)
    public SyncResult syncAll() {
        SyncResult qualifications = syncQualifications();
        SyncResult schedules = syncExamSchedules(null, null, null);
        SyncResult questions = syncOpenQuestions(null);
        return SyncResult.aggregate("all", List.of(qualifications, schedules, questions));
    }

    public record SyncResult(
            String name,
            int inserted,
            int updated,
            int skipped,
            int total,
            boolean failed,
            String message) {

        public static SyncResult failed(String message) {
            return new SyncResult("fallback", 0, 0, 0, 0, true, message);
        }

        public static SyncResult aggregate(String name, List<SyncResult> results) {
            int inserted = results.stream().mapToInt(SyncResult::inserted).sum();
            int updated = results.stream().mapToInt(SyncResult::updated).sum();
            int skipped = results.stream().mapToInt(SyncResult::skipped).sum();
            int total = results.stream().mapToInt(SyncResult::total).sum();
            boolean failed = results.stream().anyMatch(SyncResult::failed);
            String message = results.stream()
                    .map(SyncResult::message)
                    .filter(StringUtils::hasText)
                    .findFirst()
                    .orElse(null);
            return new SyncResult(name, inserted, updated, skipped, total, failed, message);
        }
    }

    private enum SyncAction {
        INSERTED, UPDATED, SKIPPED
    }
}
