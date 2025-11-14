package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.api.ExternalCertDto;
import com.OhRyue.certpilot.cert.service.CertificationQueryService;
import com.OhRyue.certpilot.cert.service.ExternalCertMapper;
import com.OhRyue.certpilot.cert.service.QnetSyncService;
import com.OhRyue.certpilot.cert.service.QnetSyncService.SyncResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Locale;
import java.util.Set;

@RestController
@RequestMapping("/api/cert/external")
@RequiredArgsConstructor
@Tag(name = "External Q-Net API")
public class ExternalCertController {

  private final CertificationQueryService certificationQueryService;
  private final ExternalCertMapper mapper;
  private final QnetSyncService qnetSyncService;

  @Operation(summary = "자격증 기본 정보 조회")
  @GetMapping("/qualifications")
  public List<ExternalCertDto.Qualification> getQualifications(@RequestParam(required = false) String seriesCd) {
    return certificationQueryService.findQualifications(seriesCd).stream()
        .map(mapper::toDto)
        .toList();
  }

  @Operation(summary = "자격증 시험 일정 조회")
  @GetMapping("/exam-schedule")
  public List<ExternalCertDto.ExamSchedule> getExamSchedules(@RequestParam(required = false, name = "year") String implYy,
                                                             @RequestParam(required = false) String qualgbCd,
                                                             @RequestParam(required = false) String jmCd) {
    return certificationQueryService.findExamSchedules(implYy, qualgbCd, jmCd).stream()
        .map(mapper::toDto)
        .toList();
  }

  @Operation(summary = "공개 문제 목록 조회")
  @GetMapping("/open-questions")
  public Page<ExternalCertDto.OpenQuestion> getOpenQuestions(@RequestParam(required = false) String jmCd,
                                                             @RequestParam(defaultValue = "1") int page,
                                                             @RequestParam(defaultValue = "20") int size) {
    return certificationQueryService.findOpenQuestions(jmCd, page, size)
        .map(mapper::toDto);
  }

  @Operation(summary = "Q-Net 동기화 수동 실행")
  @PostMapping("/sync/{source}")
  public ResponseEntity<ExternalCertDto.SyncResponse> triggerSync(@PathVariable String source,
                                                                  @RequestParam(required = false) String jmCd,
                                                                  @RequestParam(required = false) String qualgbCd,
                                                                  @RequestParam(required = false, name = "year") String implYy) {
    String normalized = source.toLowerCase(Locale.ROOT);
    SyncResult result = switch (normalized) {
      case "qualification", "qualifications" -> qnetSyncService.syncQualifications();
      case "exam", "schedule", "exam-schedule" ->
          qnetSyncService.syncExamSchedules(jmCd == null ? null : Set.of(jmCd), qualgbCd, implYy);
      case "open", "open-question", "open-questions" -> qnetSyncService.syncOpenQuestions(jmCd);
      case "all" -> qnetSyncService.syncAll();
      default -> throw new IllegalArgumentException("Unknown sync source: " + source);
    };
    ExternalCertDto.SyncResponse response = new ExternalCertDto.SyncResponse(
        result.name(), result.inserted(), result.updated(), result.skipped(), result.total(), result.failed(), result.message());
    return ResponseEntity.ok(response);
  }
}

