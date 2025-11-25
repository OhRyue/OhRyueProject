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

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

@RestController
@RequestMapping(
        value = "/api/cert/external",
        produces = MediaType.APPLICATION_JSON_VALUE
)
@RequiredArgsConstructor
@Slf4j
@Tag(name = "공공데이터포털 API", description = "data.go.kr API를 통한 자격증 정보 조회")
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

    @Operation(summary = "자격증 기본 정보 조회")
    @GetMapping("/qualifications")
    public String getQualifications(@RequestParam(required = false) String seriesCd) {
        List<ExternalCertDto.Qualification> list = certificationQueryService.findQualifications(seriesCd).stream()
                .map(mapper::toDto)
                .toList();
        return toJson(list);
    }

    @Operation(summary = "자격증 시험 일정 조회")
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

    @Operation(summary = "공개 문제 목록 조회")
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
                    공공데이터포털(data.go.kr) API만 사용합니다.
                    
                    source 값:
                    - exam/schedule/exam-schedule: 시험일정 동기화 (data.go.kr) - jmCd, qualgbCd, year 필요
                    - open/open-question/open-questions: 공개문제 동기화 (data.go.kr) - jmCd 필요
                    
                    주의: qualification 동기화는 Q-Net API가 필요하므로 현재 사용 불가
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
                    log.info("전체 동기화 실행 (exam + open, qualification 제외)");
                    SyncResult examResult = qnetSyncService.syncExamSchedules(null, null, null);
                    SyncResult openResult = qnetSyncService.syncOpenQuestions(null);
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

    @Operation(summary = "(4) 국가자격 공개문제 상세 조회 (data.go.kr)")
    @GetMapping("/open-question-detail")
    public ResponseEntity<String> getOpenQuestionDetail(@RequestParam String artlSeq) {
        try {
            String result = qnetSyncService.getOpenQuestionDetail(artlSeq);
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            log.error("공개문제 상세 조회 실패: {}", e.getMessage(), e);
            return ResponseEntity.status(500).body(toJson(Map.of("error", e.getMessage())));
        }
    }
}
