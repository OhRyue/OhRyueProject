package com.OhRyue.certpilot.cert.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.OhRyue.certpilot.cert.client.DataFeignClient;
import com.OhRyue.certpilot.cert.client.QnetFeignClient;
import com.OhRyue.certpilot.cert.config.QnetProperties;
import com.OhRyue.certpilot.cert.domain.ExamScheduleEntity;
import com.OhRyue.certpilot.cert.domain.OpenQuestionEntity;
import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import com.OhRyue.certpilot.cert.domain.QnetQualificationInfoEntity;
import com.OhRyue.certpilot.cert.dto.api.ExternalCertDto;
import com.OhRyue.certpilot.cert.dto.external.ContentsItem;
import com.OhRyue.certpilot.cert.dto.external.CurriculumItem;
import com.OhRyue.certpilot.cert.dto.external.ExamScheduleItem;
import com.OhRyue.certpilot.cert.dto.external.OpenQuestionItem;
import com.OhRyue.certpilot.cert.dto.external.QnetApiResponse;
import com.OhRyue.certpilot.cert.dto.external.QualificationItem;
import com.OhRyue.certpilot.cert.repository.ExamScheduleRepository;
import com.OhRyue.certpilot.cert.repository.OpenQuestionRepository;
import com.OhRyue.certpilot.cert.repository.QualificationRepository;
import com.OhRyue.certpilot.cert.repository.QnetQualificationInfoRepository;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;

import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class QnetSyncService {

    // 공공데이터포털 API는 한 페이지당 최대 50개까지만 조회 가능
    private static final int DEFAULT_PAGE_SIZE = 50;

    private final QnetFeignClient qnetFeignClient;
    private final DataFeignClient dataGoFeignClient;
    private final QnetProperties properties;
    private final QualificationRepository qualificationRepository;
    private final ExamScheduleRepository examScheduleRepository;
    private final OpenQuestionRepository openQuestionRepository;
    private final QnetQualificationInfoRepository qnetQualificationInfoRepository;
    private final QnetMapper mapper;
    private final ObjectMapper objectMapper;
    private final XmlMapper xmlMapper;

    public QnetSyncService(QnetFeignClient qnetFeignClient,
                           DataFeignClient dataGoFeignClient,
                           QnetProperties properties,
                           QualificationRepository qualificationRepository,
                           ExamScheduleRepository examScheduleRepository,
                           OpenQuestionRepository openQuestionRepository,
                           QnetQualificationInfoRepository qnetQualificationInfoRepository,
                           QnetMapper mapper,
                           @Qualifier("jacksonObjectMapper") ObjectMapper objectMapper,
                           XmlMapper xmlMapper) {
        this.qnetFeignClient = qnetFeignClient;
        this.dataGoFeignClient = dataGoFeignClient;
        this.properties = properties;
        this.qualificationRepository = qualificationRepository;
        this.examScheduleRepository = examScheduleRepository;
        this.openQuestionRepository = openQuestionRepository;
        this.qnetQualificationInfoRepository = qnetQualificationInfoRepository;
        this.mapper = mapper;
        this.objectMapper = objectMapper;
        this.xmlMapper = xmlMapper;
    }

    /**
     * 자격(종목) 기본 정보 동기화
     * - Q-Net(openapi.q-net.or.kr) InquiryQualInfo/getList 사용
     * 
     * @param seriesCd 계열코드 (01:기술사, 02:기능장, 03:기사, 04:기능사). null이면 모든 계열 조회
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackSyncQualification")
    @CacheEvict(value = {"cert-current", "cert-tips"}, allEntries = true)
    public SyncResult syncQualifications(String seriesCd) {
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
                    // seriesCd가 있으면 추가 (없으면 모든 계열 조회)
                    if (StringUtils.hasText(seriesCd)) {
                        extraParams.put("seriesCd", seriesCd);
                    }
                }
        );
    }
    
    /**
     * 자격(종목) 기본 정보 동기화 (모든 계열)
     */
    public SyncResult syncQualifications() {
        return syncQualifications(null);
    }

    /**
     * 국가자격 시험일정 동기화
     * - data.go.kr(B490007) /qualExamSchd/getQualExamSchdList 사용
     * - jmCds 가 여러 개면 내부에서 순차 호출하여 aggregate
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackSyncExamSchedule")
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
                    // API 응답에 jmCd가 없으므로 파라미터로 전달받은 jmCd 사용
                    String actualJmCd = null;
                    if (jmCds != null && !jmCds.isEmpty()) {
                        actualJmCd = jmCds.iterator().next();
                    } else if (StringUtils.hasText(item.getJmCd())) {
                        actualJmCd = item.getJmCd();
                    }
                    
                    if (!StringUtils.hasText(actualJmCd)) {
                        log.warn("[exam_schedule] Skipping item with empty jmCd: implYy={}, implSeq={}", 
                                item.getImplYy(), item.getImplSeq());
                        return SyncAction.SKIPPED;
                    }
                    
                    ExamScheduleEntity existing =
                            examScheduleRepository.findFirstBySourceAndImplYyAndImplSeqAndJmCd(
                                    "QNET", item.getImplYy(), item.getImplSeq(), actualJmCd);
                    log.debug("[exam_schedule] Processing item: jmCd={}, implYy={}, implSeq={}, existing={}", 
                            actualJmCd, item.getImplYy(), item.getImplSeq(), existing != null ? "found" : "not found");
                    
                    // jmCd를 설정 (API 응답에 없을 수 있으므로)
                    if (!StringUtils.hasText(item.getJmCd())) {
                        item.setJmCd(actualJmCd);
                    }
                    
                    ExamScheduleEntity entity = mapper.toExamSchedule("QNET", item, existing);
                    ExamScheduleEntity saved = examScheduleRepository.save(entity);
                    log.debug("[exam_schedule] Saved entity: id={}, jmCd={}, implYy={}, implSeq={}", 
                            saved.getId(), saved.getJmCd(), saved.getImplYy(), saved.getImplSeq());
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
                    
                    // implYy는 필수 파라미터이므로 기본값 설정
                    if (StringUtils.hasText(implYy)) {
                        extraParams.put("implYy", implYy);
                    } else {
                        // 현재 년도를 기본값으로 사용
                        String currentYear = String.valueOf(java.time.Year.now().getValue());
                        extraParams.put("implYy", currentYear);
                    }
                },
                true  // 공공데이터포털 API 사용
        );
    }

    /**
     * 공개 문제 동기화
     * - data.go.kr(B490007) /openQst/getOpenQstList 사용
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackSyncOpenQuestion")
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
                },
                true  // 공공데이터포털 API 사용
        );
    }

    /**
     * (1) 국가전문자격 시험 시행일정 정보 조회
     * - Q-Net(openapi.q-net.or.kr) InquiryTestDatesNationalProfessionalQualificationSVC/getList 사용
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackSync")
    public SyncResult getNationalProfessionalQualificationSchedule(String seriesCd) {
        Map<String, String> params = baseParams();
        params.put("seriesCd", StringUtils.hasText(seriesCd) ? seriesCd : "21");
        params.put("_type", "json");

        String raw = qnetFeignClient.getNationalProfessionalQualificationSchedule(params);
        return parseAndLog("national_professional_qualification_schedule", raw);
    }

    /**
     * (2) 국가기술자격 종목별 시험정보 조회
     * - Q-Net(openapi.q-net.or.kr) InquiryTestInformationNTQSVC/getPEList 사용
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackSync")
    public SyncResult getTechnicalQualificationInfo() {
        Map<String, String> params = baseParams();
        params.put("_type", "json");

        String raw = qnetFeignClient.getTechnicalQualificationInfo(params);
        return parseAndLog("technical_qualification_info", raw);
    }

    /**
     * (4) 국가자격 공개문제 상세 조회
     * - data.go.kr(B490007) /openQst/getOpenQst 사용
     * - artlSeq, qualgbCd 파라미터로 상세 정보 조회 (contents, fileList 포함)
     * 
     * @param artlSeq 게시물 일련번호
     * @param qualgbCd 자격구분코드 (T: 국가기술자격, P: 국가전문자격 등)
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackGetOpenQuestionDetail")
    public String getOpenQuestionDetail(String artlSeq, String qualgbCd) {
        Map<String, String> params = baseParamsForDataGo();  // 공공데이터포털은 serviceKey만 사용
        params.put("artlSeq", artlSeq);
        params.put("dataFormat", "xml");  // XML 형식 (공공데이터포털 기본 형식)
        
        // qualgbCd가 제공되지 않으면 기본값 T (국가기술자격) 사용
        if (StringUtils.hasText(qualgbCd)) {
            params.put("qualgbCd", qualgbCd);
        } else {
            params.put("qualgbCd", "T");  // 기본값: 국가기술자격
        }
        
        log.info("[open-question-detail] 공개문제 상세 조회 시작 - artlSeq: {}, qualgbCd: {}", artlSeq, params.get("qualgbCd"));
        String raw = dataGoFeignClient.getOpenQuestionDetail(params);
        
        // 첫 500자 로깅
        String rawPreview = raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw;
        log.info("[open-question-detail] API raw response (처음 500자): {}", rawPreview);
        
        return raw;
    }

    /**
     * (4) 국가자격 공개문제 상세 조회 (qualgbCd 없이 호출)
     * - 기본값 T (국가기술자격) 사용
     */
    public String getOpenQuestionDetail(String artlSeq) {
        return getOpenQuestionDetail(artlSeq, null);
    }

    public String fallbackGetOpenQuestionDetail(String artlSeq, String qualgbCd, Throwable throwable) {
        log.warn("공개문제 상세 조회 fallback triggered - artlSeq: {}, qualgbCd: {}, error: {}", artlSeq, qualgbCd, throwable.getMessage());
        throw new RuntimeException("공개문제 상세 조회 실패: " + throwable.getMessage(), throwable);
    }

    /**
     * 종목 정보 조회 (출제경향, 출제기준, 취득방법 등)
     * - data.go.kr(B490007) /qualInfo/getQualInfoList 사용
     * - 정보처리기사(jmCd=1320) 데이터 포함
     */
    @Retry(name = "datagoClient")
    @CircuitBreaker(name = "datagoClient", fallbackMethod = "fallbackGetQualificationInfo")
    public String getQualificationInfo(String jmCd) {
        Map<String, String> params = baseParamsForDataGo();  // 공공데이터포털은 serviceKey만 사용
        params.put("jmCd", jmCd);
        params.put("dataFormat", "json");  // JSON 형식 명시
        params.put("pageNo", "1");
        params.put("numOfRows", "10");
        
        log.info("[qualification-info] 종목 정보 조회 시작 - jmCd: {}, params: {}", jmCd, params);
        String raw = dataGoFeignClient.getQualificationInfo(params);
        
        // 첫 500자 로깅
        String rawPreview = raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw;
        log.info("[qualification-info] API raw response (처음 500자): {}", rawPreview);
        
        return raw;
    }

    public String fallbackGetQualificationInfo(String jmCd, Throwable throwable) {
        log.warn("종목 정보 조회 fallback triggered - jmCd: {}, error: {}", jmCd, throwable.getMessage());
        throw new RuntimeException("종목 정보 조회 실패: " + throwable.getMessage(), throwable);
    }

    /**
     * 종목별 자격정보 조회 (Q-Net API)
     * - Q-Net(openapi.q-net.or.kr) InquiryInformationTradeNTQSVC/getList 사용
     * - HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
     * - 정보처리기사(jmCd=1320)의 출제경향, 출제기준, 취득방법 등의 정보 제공
     * 
     * @param jmCd 종목코드 (예: 1320 - 정보처리기사)
     * @return JSON 형식의 구조화된 응답
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackGetInformationTradeNTQ")
    public String getInformationTradeNTQ(String jmCd) {
        Map<String, String> params = baseParams();
        params.put("jmCd", jmCd);
        params.put("_type", "xml");  // XML 형식 (Q-Net 기본 형식)
        
        log.info("[information-trade-ntq] 종목별 자격정보 조회 시작 - jmCd: {}", jmCd);
        String raw = qnetFeignClient.getInformationTradeNTQ(params);
        
        // 첫 500자 로깅
        String rawPreview = raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw;
        log.info("[information-trade-ntq] API raw response (처음 500자): {}", rawPreview);
        
        // XML을 파싱해서 JSON으로 변환 (UTF-8 인코딩 명시)
        try {
            if (raw == null || raw.isEmpty()) {
                log.warn("[information-trade-ntq] API 응답이 비어있음");
                return raw;
            }
            
            // XML 응답을 UTF-8 바이트로 읽어서 UTF-8 문자열로 변환 (Feign이 잘못된 인코딩으로 읽을 수 있음)
            byte[] bytes = raw.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1);
            String utf8Raw = new String(bytes, java.nio.charset.StandardCharsets.UTF_8);
            
            // XML을 QnetApiResponse로 파싱
            QnetApiResponse response = xmlMapper.readValue(utf8Raw, QnetApiResponse.class);
            QnetApiResponse.Response resolvedResponse = response.resolveResponse();
            if (resolvedResponse == null || resolvedResponse.getHeader() == null) {
                log.warn("[information-trade-ntq] 응답 구조가 올바르지 않음, 원본 반환");
                return raw;
            }
            
            // contents 필드에서 HTML 태그 및 엔티티 제거
            cleanContentsInResponse(resolvedResponse);
            
            // 정보처리기사(jmCd=1320)인 경우 DB에 저장
            if ("1320".equals(jmCd)) {
                saveQualificationInfoToDb(jmCd, resolvedResponse);
            }
            
            // JSON으로 변환 (UTF-8 인코딩)
            String json = objectMapper.writer().withDefaultPrettyPrinter()
                    .writeValueAsString(response);
            log.info("[information-trade-ntq] XML을 JSON으로 변환 완료 (UTF-8)");
            return json;
        } catch (Exception e) {
            log.error("[information-trade-ntq] XML 파싱/변환 중 오류 발생, 원본 XML 반환: {}", e.getMessage(), e);
            return raw;  // 오류 발생 시 원본 XML 반환
        }
    }

    public String fallbackGetInformationTradeNTQ(String jmCd, Throwable throwable) {
        log.warn("종목별 자격정보 조회 fallback triggered - jmCd: {}, error: {}", jmCd, throwable.getMessage());
        throw new RuntimeException("종목별 자격정보 조회 실패: " + throwable.getMessage(), throwable);
    }

    /**
     * (6) 자격정보 교과과정 정보 조회
     * - Q-Net(openapi.q-net.or.kr) InquiryCurriCulumSVC/getList 사용
     * - HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
     * 
     * @param jmCd 종목코드 (선택, 정보처리기사: 1320)
     * @param page 페이지 번호 (기본값: 1)
     * @param size 페이지 크기 (기본값: 20)
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackGetCurriculumInfo")
    public SyncResult getCurriculumInfo(String jmCd, int page, int size) {
        Map<String, String> params = baseParams();
        params.put("pageNo", String.valueOf(Math.max(1, page)));
        params.put("numOfRows", String.valueOf(Math.min(Math.max(1, size), 100)));
        params.put("_type", "json");
        
        // jmCd가 있으면 추가 (API 스펙에 jmCd 파라미터가 있는지 확인 필요)
        if (StringUtils.hasText(jmCd)) {
            params.put("jmCd", jmCd);
        }
        
        try {
            log.info("[curriculum] 조회 시작 - jmCd={}, page={}, size={}", jmCd, page, size);
            String raw = qnetFeignClient.getCurriculumInfo(params);
            
            Optional<QnetApiResponse> optional = parseResponse(raw);
            if (optional.isEmpty()) {
                log.warn("[curriculum] 응답 파싱 실패");
                return SyncResult.failed("응답 파싱 실패");
            }
            
            QnetApiResponse response = optional.get();
            QnetApiResponse.Response resolvedResponse = response.resolveResponse();
            if (resolvedResponse == null || resolvedResponse.getHeader() == null) {
                log.warn("[curriculum] 응답 구조가 올바르지 않음");
                return SyncResult.failed("응답 구조 오류");
            }
            
            String resultCode = resolvedResponse.getHeader().getResultCode();
            String resultMsg = resolvedResponse.getHeader().getResultMsg();
            log.info("[curriculum] API 응답: code={}, message={}", resultCode, resultMsg);
            
            if (!"00".equals(resultCode)) {
                return SyncResult.failed(String.format("API error [%s]: %s", resultCode, resultMsg));
            }
            
            QnetApiResponse.Body body = resolvedResponse.getBody();
            if (body == null) {
                return new SyncResult("curriculum", 0, 0, 0, 0, false, null);
            }
            
            List<CurriculumItem> items = convertItems(body.getItems(), CurriculumItem.class);
            log.info("[curriculum] 조회 완료 - totalCount: {}, items: {}", body.getTotalCount(), items.size());
            
            // 교과과정 정보는 로깅만 하고 저장하지 않음 (필요시 Entity 추가)
            int total = Optional.ofNullable(body.getTotalCount()).orElse(0);
            
            return new SyncResult("curriculum", 0, 0, 0, total, false, null);
        } catch (Exception e) {
            log.error("[curriculum] 조회 중 오류 발생: {}", e.getMessage(), e);
            return SyncResult.failed("조회 중 오류: " + e.getMessage());
        }
    }
    
    public SyncResult fallbackGetCurriculumInfo(String jmCd, int page, int size, Throwable throwable) {
        log.error("[curriculum] Fallback 호출: jmCd={}, page={}, size={}, error: {}", jmCd, page, size, throwable.getMessage());
        return SyncResult.failed("Q-Net API 연결 실패: " + throwable.getMessage());
    }

    /**
     * (7) Q-net 컨텐츠 관련 정보 조회 (items 반환)
     * - Q-Net(openapi.q-net.or.kr) InquiryContentsSVC/getList 사용
     * - HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
     * 
     * @param jmCd 종목코드 (선택, 정보처리기사: 1320)
     * @param page 페이지 번호 (기본값: 1)
     * @param size 페이지 크기 (기본값: 20)
     * @return JSON 문자열 (items 포함)
     */
    @Retry(name = "qnetClient")
    @CircuitBreaker(name = "qnetClient", fallbackMethod = "fallbackGetContentsInfoString")
    public String getContentsInfoAsJson(String jmCd, int page, int size) {
        Map<String, String> params = baseParams();
        params.put("pageNo", String.valueOf(Math.max(1, page)));
        params.put("numOfRows", String.valueOf(Math.min(Math.max(1, size), 100)));
        params.put("_type", "json");
        
        // 주의: Q-Net InquiryContentsSVC API는 jmCd 파라미터를 지원하지 않는 것으로 보입니다.
        // 전체 컨텐츠를 반환하므로, 클라이언트 측에서 필터링해야 합니다.
        
        try {
            log.info("[contents] 조회 시작 - jmCd={}, page={}, size={}", jmCd, page, size);
            String raw = qnetFeignClient.getContentsInfo(params);
            
            // JSON 또는 XML 응답 파싱 시도
            // Q-Net API는 _type=json 파라미터로 JSON 응답을 반환하지만, 안전하게 처리
            Optional<QnetApiResponse> optional = parseResponse(raw);
            if (optional.isEmpty()) {
                log.error("[contents] 응답 파싱 실패 - Raw response (처음 500자): {}", raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw);
                return objectMapper.writeValueAsString(Map.of("error", "응답 파싱 실패: 응답을 파싱할 수 없습니다."));
            }
            
            QnetApiResponse response = optional.get();
            QnetApiResponse.Response resolvedResponse = response.resolveResponse();
            if (resolvedResponse == null || resolvedResponse.getHeader() == null) {
                log.warn("[contents] 응답 구조가 올바르지 않음");
                return objectMapper.writeValueAsString(Map.of("error", "응답 구조 오류"));
            }
            
            String resultCode = resolvedResponse.getHeader().getResultCode();
            String resultMsg = resolvedResponse.getHeader().getResultMsg();
            log.info("[contents] API 응답: code={}, message={}", resultCode, resultMsg);
            
            if (!"00".equals(resultCode)) {
                return objectMapper.writeValueAsString(Map.of("error", String.format("API error [%s]: %s", resultCode, resultMsg)));
            }
            
            QnetApiResponse.Body body = resolvedResponse.getBody();
            if (body == null) {
                return objectMapper.writeValueAsString(new ExternalCertDto.ContentsListResponse(List.of(), 0, page, size));
            }
            
            List<ContentsItem> items = convertItems(body.getItems(), ContentsItem.class);
            log.info("[contents] 조회 완료 - totalCount: {}, items: {}", body.getTotalCount(), items.size());
            
            // jmCd가 제공되면 서버 측에서 필터링 (Q-Net API가 jmCd 파라미터를 지원하지 않음)
            if (StringUtils.hasText(jmCd)) {
                String tempQualificationName = null;
                
                // DB에서 자격증 이름 조회 시도
                Optional<QualificationEntity> qualificationOpt = qualificationRepository.findByJmCd(jmCd);
                if (qualificationOpt.isPresent()) {
                    tempQualificationName = qualificationOpt.get().getJmNm();
                }
                
                // DB 값이 깨져있거나 없는 경우 jmCd로 직접 매핑
                if (tempQualificationName == null || tempQualificationName.contains("?") || tempQualificationName.trim().isEmpty()) {
                    // jmCd에 따른 자격증 이름 하드코딩 (정보처리기사: 1320)
                    if ("1320".equals(jmCd)) {
                        tempQualificationName = "정보처리기사";
                        log.info("[contents] jmCd=1320에 대한 하드코딩된 자격증명 사용: {}", tempQualificationName);
                    } else {
                        log.warn("[contents] jmCd={}에 대한 자격증명을 찾을 수 없음", jmCd);
                    }
                }
                
                // final 변수로 복사하여 람다에서 사용 가능하도록 함
                final String qualificationName = tempQualificationName;
                
                // 필터링 시도
                if (StringUtils.hasText(qualificationName)) {
                    log.info("[contents] jmCd={} 필터링 시도 - 자격증명: {}", jmCd, qualificationName);
                    List<ContentsItem> filteredItems = items.stream()
                            .filter(item -> {
                                String title = item.getTitle() != null ? item.getTitle() : "";
                                String ctsNm = item.getCtsNm() != null ? item.getCtsNm() : "";
                                boolean matches = title.contains(qualificationName) || ctsNm.contains(qualificationName);
                                if (matches) {
                                    log.debug("[contents] 매칭된 항목: title={}, ctsNm={}", title, ctsNm);
                                }
                                return matches;
                            })
                            .collect(java.util.stream.Collectors.toList());
                    
                    if (!filteredItems.isEmpty()) {
                        items = filteredItems;
                        log.info("[contents] 필터링 성공 - 필터링 후 items: {} 개", items.size());
                    } else {
                        log.warn("[contents] jmCd={}에 해당하는 컨텐츠를 찾을 수 없음 (필터링된 결과: 0개)", jmCd);
                        // 필터링 결과가 없으면 빈 배열 반환
                        items = List.of();
                    }
                }
            }
            
            // items를 ExternalCertDto.ContentsItem으로 변환
            List<ExternalCertDto.ContentsItem> dtoItems = items.stream()
                    .map(item -> new ExternalCertDto.ContentsItem(
                            item.getCtsId(),
                            item.getCtsNm(),
                            item.getDeptNm(),
                            item.getPubYnCcd(),
                            item.getTitle()
                    ))
                    .toList();
            
            // ContentsListResponse 생성 (필터링된 경우 totalCount 조정)
            int finalTotalCount = StringUtils.hasText(jmCd) ? dtoItems.size() : Optional.ofNullable(body.getTotalCount()).orElse(0);
            ExternalCertDto.ContentsListResponse responseDto = new ExternalCertDto.ContentsListResponse(
                    dtoItems,
                    finalTotalCount,
                    Optional.ofNullable(body.getPageNo()).orElse(page),
                    Optional.ofNullable(body.getNumOfRows()).orElse(size)
            );
            
            // JSON으로 변환하여 반환
            String json = objectMapper.writeValueAsString(responseDto);
            log.info("[contents] JSON 변환 완료 - items: {}", dtoItems.size());
            return json;
        } catch (Exception e) {
            log.error("[contents] 조회 중 오류 발생: {}", e.getMessage(), e);
            throw new RuntimeException("조회 중 오류: " + e.getMessage(), e);
        }
    }
    
    public String fallbackGetContentsInfoString(String jmCd, int page, int size, Throwable throwable) {
        log.error("[contents] Fallback 호출: jmCd={}, page={}, size={}, error: {}", jmCd, page, size, throwable.getMessage());
        try {
            return objectMapper.writeValueAsString(Map.of("error", "Q-Net API 연결 실패: " + throwable.getMessage()));
        } catch (Exception e) {
            return "{\"error\": \"Q-Net API 연결 실패\"}";
        }
    }

    /**
     * 단순 조회용 파싱 및 로깅 (저장하지 않음)
     */
    private SyncResult parseAndLog(String name, String raw) {
        Optional<QnetApiResponse> optional = parseResponse(raw);
        if (optional.isEmpty()) {
            log.warn("[{}] Unable to parse response", name);
            return SyncResult.failed("Failed to parse response");
        }

        QnetApiResponse response = optional.get();
        if (response.getResponse() == null || response.getResponse().getHeader() == null) {
            log.warn("[{}] Missing header", name);
            return SyncResult.failed("Missing header");
        }

        String resultCode = response.getResponse().getHeader().getResultCode();
        if (!"00".equals(resultCode)) {
            log.warn("[{}] API returned non-success code {} ({})",
                    name,
                    resultCode,
                    response.getResponse().getHeader().getResultMsg());
            return SyncResult.failed(response.getResponse().getHeader().getResultMsg());
        }

        log.info("[{}] Successfully fetched data", name);
        return new SyncResult(name, 0, 0, 0, 0, false, null);
    }

    /* ==================== Resilience4j Fallback ==================== */

    // 기본 fallback 메서드
    public SyncResult fallbackSync(Throwable throwable) {
        log.warn("Q-Net sync fallback triggered: {}", throwable.getMessage());
        return SyncResult.failed(throwable.getMessage());
    }

    // 각 메서드별 전용 fallback (Resilience4j가 정확히 매칭할 수 있도록)
    public SyncResult fallbackSyncExamSchedule(Set<String> jmCds, String qualgbCd, String implYy, Throwable throwable) {
        return fallbackSync(throwable);
    }

    public SyncResult fallbackSyncOpenQuestion(String jmCd, Throwable throwable) {
        return fallbackSync(throwable);
    }

    public SyncResult fallbackSyncQualification(String seriesCd, Throwable throwable) {
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
        return fetchPaged(name, caller, itemClass, handler, extraParamConsumer, false);
    }

    private <T> SyncResult fetchPaged(String name,
                                      Function<Map<String, String>, String> caller,
                                      Class<T> itemClass,
                                      Function<T, SyncAction> handler,
                                      java.util.function.Consumer<Map<String, String>> extraParamConsumer,
                                      boolean useDataGo) {
        int pageNo = 1;
        int inserted = 0;
        int updated = 0;
        int skipped = 0;
        int totalCount = 0;

        // 동기화 시작 파라미터 로깅
        Map<String, String> firstPageParams = useDataGo ? baseParamsForDataGo() : baseParams();
        firstPageParams.put("pageNo", "1");
        firstPageParams.put("numOfRows", String.valueOf(DEFAULT_PAGE_SIZE));
        extraParamConsumer.accept(firstPageParams);
        log.info("[{}] 동기화 시작 - 파라미터: {}", name, firstPageParams);

        while (true) {
            Map<String, String> params = useDataGo ? baseParamsForDataGo() : baseParams();
            params.put("pageNo", String.valueOf(pageNo));
            params.put("numOfRows", String.valueOf(DEFAULT_PAGE_SIZE));

            extraParamConsumer.accept(params);

            String raw = caller.apply(params);
            
            // 첫 번째 페이지는 INFO 레벨로 로깅 (디버깅용), 나머지는 DEBUG
            if (pageNo == 1) {
                String rawPreview = raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw;
                log.info("[{}] API raw response (처음 500자): {}", name, rawPreview);
            } else {
                log.debug("[{}] API raw response (page {}): {}", name, pageNo, raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw);
            }
            
            Optional<QnetApiResponse> optional = parseResponse(raw);
            if (optional.isEmpty()) {
                log.warn("[{}] Unable to parse response for page {}. Raw response: {}", name, pageNo, raw != null && raw.length() > 1000 ? raw.substring(0, 1000) + "..." : raw);
                break;
            }

            QnetApiResponse response = optional.get();
            QnetApiResponse.Response resolvedResponse = response.resolveResponse();
            if (resolvedResponse == null || resolvedResponse.getHeader() == null) {
                log.warn("[{}] Missing header for page {}. Response structure: {}", name, pageNo, raw != null && raw.length() > 500 ? raw.substring(0, 500) + "..." : raw);
                break;
            }

            String resultCode = resolvedResponse.getHeader().getResultCode();
            String resultMsg = resolvedResponse.getHeader().getResultMsg();
            log.info("[{}] API response code: {}, message: {} (page {})", name, resultCode, resultMsg, pageNo);
            
            if (!"00".equals(resultCode)) {
                log.error("[{}] API returned non-success code {} ({}) on page {}. Full response: {}",
                        name,
                        resultCode,
                        resultMsg,
                        pageNo,
                        raw != null && raw.length() > 1000 ? raw.substring(0, 1000) + "..." : raw);
                // resultCode가 "00"이 아니면 실패로 처리하고 중단
                return SyncResult.failed(String.format("API error [%s]: %s", resultCode, resultMsg));
            }

            QnetApiResponse.Body body = resolvedResponse.getBody();
            if (body == null) {
                break;
            }

            totalCount = Optional.ofNullable(body.getTotalCount()).orElse(0);
            log.info("[{}] Total count: {}", name, totalCount);
            
            List<T> items = convertItems(body.getItems(), itemClass);
            log.info("[{}] Converted {} items from page {} (page: {}, numOfRows: {})", name, items.size(), pageNo, body.getPageNo(), body.getNumOfRows());
            
            if (items.isEmpty()) {
                log.info("[{}] No items found on page {}, stopping pagination", name, pageNo);
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
     * - 주의: Q-Net API 전용 (openapi.q-net.or.kr)
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

    /**
     * 공공데이터포털 API용 기본 파라미터 – serviceKey만 세팅
     * - data.go.kr API는 serviceKey만 필요하며, ServiceKey를 넣으면 중복 오류 발생
     */
    private Map<String, String> baseParamsForDataGo() {
        Map<String, String> params = new HashMap<>();
        String key = Optional.ofNullable(properties.getKey()).orElse("");

        if (StringUtils.hasText(key)) {
            params.put("serviceKey", key);  // serviceKey만 사용
        }

        return params;
    }

    private Optional<QnetApiResponse> parseResponse(String raw) {
        if (!StringUtils.hasText(raw)) {
            return Optional.empty();
        }
        try {
            JsonNode root = objectMapper.readTree(raw);
            QnetApiResponse apiResponse = new QnetApiResponse();
            JsonNode responseNode = root.has("response") ? root.get("response") : null;
            JsonNode headerNode = responseNode != null && responseNode.has("header")
                    ? responseNode.get("header")
                    : root.get("header");
            JsonNode bodyNode = responseNode != null && responseNode.has("body")
                    ? responseNode.get("body")
                    : root.get("body");
            boolean hasResponseNode = responseNode != null && !responseNode.isMissingNode();
            boolean hasHeaderNode = headerNode != null && !headerNode.isMissingNode();
            boolean hasBodyNode = bodyNode != null && !bodyNode.isMissingNode();
            log.debug("Parsed API response nodes - response:{}, header:{}, body:{}", hasResponseNode, hasHeaderNode, hasBodyNode);

            if (responseNode != null) {
                apiResponse.setResponse(objectMapper.treeToValue(responseNode, QnetApiResponse.Response.class));
            } else {
                QnetApiResponse.Response fallback = new QnetApiResponse.Response();
                if (headerNode != null && !headerNode.isMissingNode()) {
                    QnetApiResponse.Header header = new QnetApiResponse.Header();
                    header.setResultCode(headerNode.path("resultCode").asText(null));
                    header.setResultMsg(headerNode.path("resultMsg").asText(null));
                    fallback.setHeader(header);
                    apiResponse.setRootHeader(fallback.getHeader());
                }
                if (bodyNode != null && !bodyNode.isMissingNode()) {
                    QnetApiResponse.Body body = new QnetApiResponse.Body();
                    body.setItems(bodyNode.get("items"));
                    if (bodyNode.hasNonNull("numOfRows")) {
                        body.setNumOfRows(bodyNode.get("numOfRows").asInt());
                    }
                    if (bodyNode.hasNonNull("pageNo")) {
                        body.setPageNo(bodyNode.get("pageNo").asInt());
                    }
                    if (bodyNode.hasNonNull("totalCount")) {
                        body.setTotalCount(bodyNode.get("totalCount").asInt());
                    }
                    fallback.setBody(body);
                    apiResponse.setRootBody(fallback.getBody());
                }
                if (fallback.getHeader() != null || fallback.getBody() != null) {
                    apiResponse.setResponse(fallback);
                }
            }

            if (apiResponse.resolveResponse() != null) {
                return Optional.of(apiResponse);
            }
            log.warn("Unable to resolve API response structure - responseNode:{}, headerNode:{}, bodyNode:{}",
                    hasResponseNode, hasHeaderNode, hasBodyNode);
        } catch (JsonProcessingException e) {
            log.warn("Failed to parse response as JSON: {}", e.getOriginalMessage());
            // JSON 파싱 실패 시, 응답이 XML로 시작하는 경우에만 XML 파싱 시도
            String trimmed = raw.trim();
            if (trimmed.startsWith("<") || trimmed.startsWith("<?xml")) {
                try {
                    log.debug("Attempting to parse as XML (response starts with '<')");
                    return Optional.of(xmlMapper.readValue(raw, QnetApiResponse.class));
                } catch (Exception xmlEx) {
                    log.debug("Failed to parse response as XML: {}", xmlEx.getMessage());
                }
            }
        } catch (Exception e) {
            log.warn("Unexpected error while parsing response: {}", e.getMessage());
        }

        return Optional.empty();
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
        // 공공데이터포털 API만 사용: qualification 제외
        log.info("전체 동기화 실행 (공공데이터포털 API만 사용 - exam + open)");
        SyncResult schedules = syncExamSchedules(null, null, null);
        SyncResult questions = syncOpenQuestions(null);
        return SyncResult.aggregate("all", List.of(schedules, questions));
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

    /**
     * 응답의 contents 필드에서 HTML 태그와 엔티티를 제거하여 순수 텍스트만 남깁니다.
     */
    private void cleanContentsInResponse(QnetApiResponse.Response response) {
        if (response == null || response.getBody() == null) {
            return;
        }
        
        JsonNode itemsNode = response.getBody().getItems();
        if (itemsNode == null || itemsNode.isNull()) {
            return;
        }
        
        JsonNode itemNode = itemsNode.has("item") ? itemsNode.get("item") : itemsNode;
        if (itemNode == null || itemNode.isNull()) {
            return;
        }
        
        // item이 배열인 경우
        if (itemNode.isArray()) {
            for (JsonNode item : itemNode) {
                cleanContentsInItem(item);
            }
        } else {
            // item이 단일 객체인 경우
            cleanContentsInItem(itemNode);
        }
        
        // JsonNode는 직접 수정되므로 별도로 설정할 필요 없음
    }
    
    /**
     * 개별 item의 contents 필드를 정리합니다.
     */
    private void cleanContentsInItem(JsonNode item) {
        if (item == null || !item.has("contents")) {
            return;
        }
        
        JsonNode contentsNode = item.get("contents");
        if (contentsNode == null || !contentsNode.isTextual()) {
            return;
        }
        
        String contents = contentsNode.asText();
        if (contents == null || contents.isEmpty()) {
            return;
        }
        
        // HTML 태그 제거 및 HTML 엔티티 디코딩
        String cleaned = cleanHtmlContent(contents);
        
        // JsonNode는 불변 객체이므로, ObjectNode로 변환하여 수정
        if (item.isObject()) {
            ((com.fasterxml.jackson.databind.node.ObjectNode) item).put("contents", cleaned);
        }
    }
    
    /**
     * HTML 태그와 CSS 스타일 블록을 제거하고 HTML 엔티티를 디코딩합니다.
     */
    private String cleanHtmlContent(String html) {
        if (html == null || html.isEmpty()) {
            return html;
        }
        
        // 1. HTML 태그 제거 (정규식 사용)
        String cleaned = html.replaceAll("</?[^>]+>", "");
        
        // 2. CSS 스타일 블록 제거 (예: "BODY { ... }", "P { ... }", "LI { ... }")
        // 선택자 이름(대문자로 시작)과 중괄호로 감싸진 스타일 블록 제거
        cleaned = cleaned.replaceAll("[A-Z]+\\s*\\{[^}]*\\}", "");
        
        // 3. HTML 엔티티 디코딩
        cleaned = decodeHtmlEntities(cleaned);
        
        // 4. 연속된 공백을 하나로 줄이기
        cleaned = cleaned.replaceAll("\\s+", " ").trim();
        
        return cleaned;
    }
    
    /**
     * HTML 엔티티를 디코딩합니다.
     * 주요 엔티티와 숫자 엔티티를 처리합니다.
     */
    private String decodeHtmlEntities(String text) {
        if (text == null || text.isEmpty()) {
            return text;
        }
        
        String result = text;
        
        // 기본 HTML 엔티티 (순서 중요: &amp;를 먼저 처리하면 안 됨)
        result = result.replace("&lt;", "<");
        result = result.replace("&gt;", ">");
        result = result.replace("&quot;", "\"");
        result = result.replace("&apos;", "'");
        result = result.replace("&nbsp;", " ");
        
        // 숫자 엔티티 처리 (&#숫자; 같은 경우, 예: &#9312;)
        java.util.regex.Pattern decimalPattern = java.util.regex.Pattern.compile("&#(\\d+);");
        java.util.regex.Matcher decimalMatcher = decimalPattern.matcher(result);
        StringBuffer decimalBuffer = new StringBuffer();
        while (decimalMatcher.find()) {
            try {
                int codePoint = Integer.parseInt(decimalMatcher.group(1));
                decimalMatcher.appendReplacement(decimalBuffer, String.valueOf((char) codePoint));
            } catch (NumberFormatException e) {
                // 변환 실패 시 원본 유지
                decimalMatcher.appendReplacement(decimalBuffer, decimalMatcher.group(0));
            }
        }
        decimalMatcher.appendTail(decimalBuffer);
        result = decimalBuffer.toString();
        
        // 16진수 엔티티 처리 (&#x숫자;)
        java.util.regex.Pattern hexPattern = java.util.regex.Pattern.compile("&#x([0-9a-fA-F]+);");
        java.util.regex.Matcher hexMatcher = hexPattern.matcher(result);
        StringBuffer hexBuffer = new StringBuffer();
        while (hexMatcher.find()) {
            try {
                int codePoint = Integer.parseInt(hexMatcher.group(1), 16);
                hexMatcher.appendReplacement(hexBuffer, String.valueOf((char) codePoint));
            } catch (NumberFormatException e) {
                // 변환 실패 시 원본 유지
                hexMatcher.appendReplacement(hexBuffer, hexMatcher.group(0));
            }
        }
        hexMatcher.appendTail(hexBuffer);
        result = hexBuffer.toString();
        
        // &amp;는 마지막에 처리 (다른 엔티티가 깨지지 않도록)
        result = result.replace("&amp;", "&");
        
        return result;
    }
    
    /**
     * Q-Net 자격정보를 DB에 저장합니다.
     * 정보처리기사(jmCd=1320) 데이터만 저장합니다.
     */
    private void saveQualificationInfoToDb(String jmCd, QnetApiResponse.Response response) {
        if (response == null || response.getBody() == null) {
            log.warn("[save-qualification-info] 응답 body가 없음");
            return;
        }
        
        JsonNode itemsNode = response.getBody().getItems();
        if (itemsNode == null || itemsNode.isNull()) {
            log.warn("[save-qualification-info] items가 없음");
            return;
        }
        
        JsonNode itemNode = itemsNode.has("item") ? itemsNode.get("item") : itemsNode;
        if (itemNode == null || itemNode.isNull()) {
            log.warn("[save-qualification-info] item이 없음");
            return;
        }
        
        // 기존 데이터 삭제 (jmCd=1320인 경우만)
        if ("1320".equals(jmCd)) {
            qnetQualificationInfoRepository.deleteByJmCd(jmCd);
            log.info("[save-qualification-info] 기존 데이터 삭제 완료: jmCd={}", jmCd);
        }
        
        List<QnetQualificationInfoEntity> entities = new ArrayList<>();
        
        // item이 배열인 경우
        if (itemNode.isArray()) {
            for (JsonNode item : itemNode) {
                QnetQualificationInfoEntity entity = convertToEntity(jmCd, item);
                if (entity != null) {
                    entities.add(entity);
                }
            }
        } else {
            // item이 단일 객체인 경우
            QnetQualificationInfoEntity entity = convertToEntity(jmCd, itemNode);
            if (entity != null) {
                entities.add(entity);
            }
        }
        
        if (!entities.isEmpty()) {
            qnetQualificationInfoRepository.saveAll(entities);
            log.info("[save-qualification-info] 저장 완료: jmCd={}, count={}", jmCd, entities.size());
        } else {
            log.warn("[save-qualification-info] 저장할 데이터가 없음: jmCd={}", jmCd);
        }
    }
    
    /**
     * JsonNode를 QnetQualificationInfoEntity로 변환합니다.
     */
    private QnetQualificationInfoEntity convertToEntity(String jmCd, JsonNode item) {
        try {
            QnetQualificationInfoEntity entity = new QnetQualificationInfoEntity();
            entity.setJmCd(jmCd);
            entity.setInfogb(item.has("infogb") ? item.get("infogb").asText(null) : null);
            entity.setContents(item.has("contents") ? item.get("contents").asText(null) : null);
            entity.setJmfldnm(item.has("jmfldnm") ? item.get("jmfldnm").asText(null) : null);
            entity.setMdobligfldcd(item.has("mdobligfldcd") ? item.get("mdobligfldcd").asText(null) : null);
            entity.setMdobligfldnm(item.has("mdobligfldnm") ? item.get("mdobligfldnm").asText(null) : null);
            entity.setObligfldcd(item.has("obligfldcd") ? item.get("obligfldcd").asText(null) : null);
            entity.setObligfldnm(item.has("obligfldnm") ? item.get("obligfldnm").asText(null) : null);
            return entity;
        } catch (Exception e) {
            log.error("[save-qualification-info] Entity 변환 실패: {}", e.getMessage(), e);
            return null;
        }
    }
}
