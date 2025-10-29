package com.OhRyue.certpilot.certs.web;

import com.OhRyue.certpilot.certs.dto.CertificateDetailDto;
import com.OhRyue.certpilot.certs.dto.CertificateSummaryDto;
import com.OhRyue.certpilot.certs.service.CertificateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Certs", description = "자격증 정보 API")
@RestController
@RequestMapping(path = "/api/certs", produces = MediaType.APPLICATION_JSON_VALUE)
public class CertificateController {

  private final CertificateService service;

  public CertificateController(CertificateService service) {
    this.service = service;
  }

  @Operation(summary = "자격증 목록 조회", description = "분야 필터 및 페이지네이션 지원")
  @GetMapping
  public Page<CertificateSummaryDto> list(
      @Parameter(description = "분야 필터 (예: IT, 회계)")
      @RequestParam(required = false) String field,
      @Parameter(description = "페이지 번호 (기본값: 0)")
      @RequestParam(defaultValue = "0") int page,
      @Parameter(description = "페이지 크기 (기본값: 20)")
      @RequestParam(defaultValue = "20") int size
  ) {
    if (page < 0) page = 0;
    if (size <= 0 || size > 100) size = 20;
    Pageable pageable = PageRequest.of(page, size);
    return service.findAll(field, pageable);
  }

  @Operation(summary = "자격증 상세 조회")
  @GetMapping("/{id}")
  public CertificateDetailDto get(@PathVariable Long id) {
    return service.getById(id);
  }

  @Operation(summary = "자격증 검색", description = "자동완성/추천 용도")
  @GetMapping("/search")
  public List<CertificateSummaryDto> search(
      @Parameter(description = "검색어")
      @RequestParam String keyword,
      @Parameter(description = "결과 개수 (기본값: 10)")
      @RequestParam(defaultValue = "10") int limit
  ) {
    return service.search(keyword, limit);
  }
}

