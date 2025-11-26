package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.api.ExternalCertDto;
import com.OhRyue.certpilot.cert.service.CertificationQueryService;
import com.OhRyue.certpilot.cert.service.ExternalCertMapper;
import com.OhRyue.certpilot.cert.service.QnetSyncService;
import com.OhRyue.certpilot.cert.service.QnetSyncService.SyncResult;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.OhRyue.certpilot.cert.domain.QualificationEntity;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping(
        value = "/api/cert/external",
        produces = MediaType.APPLICATION_JSON_VALUE
)
@RequiredArgsConstructor
@Slf4j
@Tag(name = "공공데이터포털 API (전용)", description = """
        공공데이터포털(data.go.kr) API 전용 자격증 정보 조회/동기화
        
        ✅ 사용 가능 (data.go.kr API):
        - 시험일정 조회/동기화
        - 공개문제 조회/동기화
        - 공개문제 상세 조회 (contents, fileList 포함)
        - 종목 정보 조회 (출제경향, 출제기준, 취득방법)
        
        ✅ Q-Net API (HTTP 연결 가능):
        - 종목별 자격정보 조회 (/qualification-info-qnet) - HTTP (포트 80) 연결 가능
        - 컨텐츠 정보 조회 (/contents-info) - HTTP (포트 80) 연결 가능
        - 교과과정 정보 조회 (/curriculum-info) - HTTP (포트 80) 연결 가능
        """)
public class ExternalCertController {

    private final CertificationQueryService certificationQueryService;
    private final ExternalCertMapper mapper;
    private final QnetSyncService qnetSyncService;
    private final ObjectMapper objectMapper;

    private String toJson(Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("JSON 직렬화 실패", e);
        }
    }

    @Operation(summary = "자격증 기본 정보 조회",
            description = "DB에 저장된 자격증 기본 정보를 조회합니다. (동기화가 필요한 경우 Q-Net API 필요 - 현재 사용 불가)")
    @GetMapping("/qualifications")
    public String getQualifications(@RequestParam(required = false) String seriesCd) {
        List<ExternalCertDto.Qualification> list = certificationQueryService.findQualifications(seriesCd).stream()
                .map(mapper::toDto)
                .toList();
        return toJson(list);
    }

    @Operation(summary = "자격증 시험 일정 조회 (✅ data.go.kr API - 정상 작동)",
            description = "DB에 저장된 시험일정을 조회합니다. data.go.kr API를 통해 동기화됩니다.")
    @GetMapping("/exam-schedule")
    public String getExamSchedules(@RequestParam(required = false, name = "year") String implYy,
                                   @RequestParam(required = false) String qualgbCd,
                                   @RequestParam(required = false) String jmCd) {
        try {
            List<ExternalCertDto.ExamSchedule> list = certificationQueryService
                    .findExamSchedules(implYy, qualgbCd, jmCd).stream()
                    .map(mapper::toDto)
                    .toList();
            log.info("조회된 시험일정 수: {} (implYy={}, qualgbCd={}, jmCd={})", list.size(), implYy, qualgbCd, jmCd);
            return toJson(list);
        } catch (Exception e) {
            log.error("시험일정 조회 중 오류 발생", e);
            throw new RuntimeException("시험일정 조회 실패: " + e.getMessage(), e);
        }
    }

    @Operation(summary = "공개 문제 목록 조회 (✅ data.go.kr API - 정상 작동)",
            description = "DB에 저장된 공개문제를 조회합니다. data.go.kr API를 통해 동기화됩니다.")
    @GetMapping("/open-questions")
    public String getOpenQuestions(@RequestParam(required = false) String jmCd,
                                   @RequestParam(defaultValue = "1") int page,
                                   @RequestParam(defaultValue = "20") int size) {
        Page<ExternalCertDto.OpenQuestion> resultPage =
                certificationQueryService.findOpenQuestions(jmCd, page, size)
                        .map(mapper::toDto);
        return toJson(resultPage);
    }

    @Operation(summary = "공공데이터포털 API 동기화 수동 실행",
            description = """
                    ✅ 공공데이터포털(data.go.kr) API만 사용합니다.
                    
                    source 값:
                    - exam/schedule/exam-schedule: 시험일정 동기화 (✅ data.go.kr) - jmCd, qualgbCd, year 필요
                    - open/open-question/open-questions: 공개문제 동기화 (✅ data.go.kr) - jmCd 필요
                    - all: 전체 동기화 (exam + open)
                    
                    ❌ 주의: qualification 동기화는 Q-Net API가 필요하므로 현재 네트워크 연결 불가로 사용 불가
                    """)
    @PostMapping("/sync/{source}")
    public ResponseEntity<String> triggerSync(@PathVariable String source,
                                              @RequestParam(required = false) String jmCd,
                                              @RequestParam(required = false) String qualgbCd,
                                              @RequestParam(required = false, name = "year") String implYy,
                                              @RequestParam(required = false) String seriesCd) {
        try {
            String normalized = source.toLowerCase(Locale.ROOT);
            SyncResult result;
            
            switch (normalized) {
                case "qualification", "qualifications" -> {
                    log.warn("qualification 동기화는 Q-Net API가 필요합니다. 현재 공공데이터포털 API만 사용 가능합니다.");
                    throw new UnsupportedOperationException("qualification 동기화는 Q-Net API가 필요합니다. 공공데이터포털 API만 사용 가능합니다.");
                }
                case "exam", "schedule", "exam-schedule" ->
                        result = qnetSyncService.syncExamSchedules(jmCd == null ? null : Set.of(jmCd), qualgbCd, implYy);
                case "open", "open-question", "open-questions" -> 
                        result = qnetSyncService.syncOpenQuestions(jmCd);
                case "all" -> {
                    // qualification 제외하고 exam과 open만 동기화
                    // year 파라미터를 기본값 2025로 고정
                    String defaultYear = (implYy != null && !implYy.isEmpty()) ? implYy : "2025";
                    log.info("전체 동기화 실행 (exam + open, qualification 제외) - jmCd: {}, year: {}", jmCd, defaultYear);
                    Set<String> jmCdSet = jmCd != null ? Set.of(jmCd) : null;
                    SyncResult examResult = qnetSyncService.syncExamSchedules(jmCdSet, qualgbCd, defaultYear);
                    SyncResult openResult = qnetSyncService.syncOpenQuestions(jmCd);
                    result = SyncResult.aggregate("all", List.of(examResult, openResult));
                }
                default -> throw new IllegalArgumentException("Unknown sync source: " + source);
            }
            
            ExternalCertDto.SyncResponse dto = new ExternalCertDto.SyncResponse(
                    result.name(), result.inserted(), result.updated(), result.skipped(),
                    result.total(), result.failed(), result.message());
            return ResponseEntity.ok(toJson(dto));
        } catch (Exception e) {
            log.error("공공데이터포털 API sync failed for source: {}, error: {}", source, e.getMessage(), e);
            ExternalCertDto.SyncResponse errorDto = new ExternalCertDto.SyncResponse(
                    "error", 0, 0, 0, 0, true, 
                    "동기화 중 오류 발생: " + e.getMessage());
            return ResponseEntity.status(500).body(toJson(errorDto));
        }
    }

    @Operation(summary = "공개문제 상세 조회 (✅ data.go.kr API - 정상 작동)",
            description = """
                    공개문제 상세 정보를 조회합니다. data.go.kr API를 통해 실시간으로 조회됩니다.
                    - contents: HTML 형식의 상세 내용
                    - fileList: 첨부파일 목록 (PDF 등)
                    - qualgbCd: 자격구분코드 (T: 국가기술자격, P: 국가전문자격 등), 기본값: T
                    """)
    @GetMapping("/open-question-detail")
    public ResponseEntity<String> getOpenQuestionDetail(
            @RequestParam String artlSeq,
            @RequestParam(required = false) String qualgbCd) {
        try {
            String result = qnetSyncService.getOpenQuestionDetail(artlSeq, qualgbCd);
            log.info("공개문제 상세 조회 성공: artlSeq={}, qualgbCd={}", artlSeq, qualgbCd != null ? qualgbCd : "T(기본값)");
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            log.error("공개문제 상세 조회 실패: artlSeq={}, qualgbCd={}, error: {}", artlSeq, qualgbCd, e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }

    @Operation(summary = "종목 정보 조회 (✅ DB 조회 - 정상 작동)",
            description = """
                    종목코드(jmCd)로 출제경향, 출제기준, 취득방법 등의 정보를 DB에서 조회합니다.
                    정보처리기사 데이터 포함 (jmCd=1320)
                    
                    공공데이터포털 API가 500 에러를 반환하므로, DB에 저장된 데이터를 반환합니다.
                    """)
    @GetMapping("/qualification-info")
    public ResponseEntity<String> getQualificationInfo(@RequestParam String jmCd) {
        try {
            // DB에서 조회 (공공데이터포털 API가 500 에러를 반환하므로 DB 조회로 변경)
            Optional<QualificationEntity> qualificationOpt = certificationQueryService.findQualificationByJmCd(jmCd);
            if (!qualificationOpt.isPresent()) {
                log.warn("종목 정보를 찾을 수 없음: jmCd={}", jmCd);
                return ResponseEntity.status(404).body(toJson(Map.of("error", "종목 정보를 찾을 수 없습니다. jmCd: " + jmCd)));
            }
            
            ExternalCertDto.Qualification qualification = mapper.toDto(qualificationOpt.get());
            log.info("종목 정보 조회 성공 (DB): jmCd={}", jmCd);
            return ResponseEntity.ok(toJson(qualification));
        } catch (Exception e) {
            log.error("종목 정보 조회 실패: jmCd={}, error: {}", jmCd, e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }

    @Operation(summary = "종목별 자격정보 조회 (✅ Q-Net API - HTTP 연결 가능)",
            description = """
                    Q-Net API를 사용하여 종목별 자격정보를 조회합니다.
                    ✅ HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
                    
                    정보처리기사(jmCd=1320)의 출제경향, 출제기준, 취득방법 등의 정보를 조회할 수 있습니다.
                    """)
    @GetMapping("/qualification-info-qnet")
    public ResponseEntity<String> getQualificationInfoQnet(@RequestParam String jmCd) {
        try {
            String result = qnetSyncService.getInformationTradeNTQ(jmCd);
            log.info("종목별 자격정보 조회 성공 (Q-Net): jmCd={}", jmCd);
            // XML을 JSON으로 변환한 결과를 반환 (UTF-8 인코딩 명시)
            return ResponseEntity.ok()
                    .contentType(MediaType.APPLICATION_JSON)
                    .header("Content-Type", "application/json; charset=UTF-8")
                    .body(result);
        } catch (Exception e) {
            log.error("종목별 자격정보 조회 실패 (Q-Net): jmCd={}, error: {}", jmCd, e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }

    @Operation(summary = "Q-Net 컨텐츠 정보 조회 (✅ Q-Net API - HTTP 연결 가능)",
            description = """
                    Q-Net API를 사용하여 컨텐츠 정보를 조회합니다.
                    ✅ HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
                    
                    ⚠️ 주의: Q-Net InquiryContentsSVC API는 jmCd 파라미터를 지원하지 않습니다.
                    전체 컨텐츠 정보를 반환하므로, 클라이언트 측에서 필터링해야 합니다.
                    """)
    @GetMapping("/contents-info")
    public ResponseEntity<String> getContentsInfo(
            @RequestParam(required = false) String jmCd,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "20") int size) {
        try {
            String result = qnetSyncService.getContentsInfoAsJson(jmCd, page, size);
            log.info("Q-Net 컨텐츠 정보 조회 성공: jmCd={}, page={}, size={}", jmCd, page, size);
            return ResponseEntity.ok()
                    .contentType(MediaType.APPLICATION_JSON)
                    .header("Content-Type", "application/json; charset=UTF-8")
                    .body(result);
        } catch (Exception e) {
            log.error("Q-Net 컨텐츠 정보 조회 실패: jmCd={}, error: {}", jmCd, e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }

    @Operation(summary = "Q-Net 교과과정 정보 조회 (✅ Q-Net API - HTTP 연결 가능)",
            description = """
                    Q-Net API를 사용하여 교과과정 정보를 조회합니다.
                    ✅ HTTP (포트 80) 연결 가능 (HTTPS는 연결 불가)
                    
                    정보처리기사 관련 교과과정 포함
                    """)
    @GetMapping("/curriculum-info")
    public ResponseEntity<String> getCurriculumInfo(
            @RequestParam(required = false) String jmCd,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "20") int size) {
        try {
            SyncResult result = qnetSyncService.getCurriculumInfo(jmCd, page, size);
            log.info("Q-Net 교과과정 정보 조회 성공: jmCd={}, page={}, size={}", jmCd, page, size);
            return ResponseEntity.ok(toJson(result));
        } catch (Exception e) {
            log.error("Q-Net 교과과정 정보 조회 실패: jmCd={}, error: {}", jmCd, e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }

    // ============================================
    // 시연용: 불필요한 Q-Net API 엔드포인트 제거
    // (1), (2) 국가전문자격/기술자격 시험일정 → (5) 시험일정 조회 API로 대체됨
    // ============================================
}
