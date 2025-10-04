package pack.certs;

import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import pack.certs.dto.CertificateDetailDto;
import pack.certs.dto.CertificateSummaryDto;

import java.util.List;

@RestController
@RequestMapping(path = "/certs", produces = MediaType.APPLICATION_JSON_VALUE)
public class CertificateController {

  private final CertificateService service;

  public CertificateController(CertificateService service) {
    this.service = service;
  }

  // 목록 + 분야필터 + 페이지네이션
  // GET /certs?field=IT&page=0&size=20
  @GetMapping
  public Page<CertificateSummaryDto> list(
      @RequestParam(required = false) String field,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size
  ) {
    if (page < 0) page = 0;
    if (size <= 0 || size > 100) size = 20;
    Pageable pageable = PageRequest.of(page, size);
    return service.findAll(field, pageable);
  }

  // 상세
  // GET /certs/{id}
  @GetMapping("/{id}")
  public CertificateDetailDto get(@PathVariable Long id) {
    return service.getById(id);
  }

  // 검색 (자동완성/추천 용)
  // GET /certs/search?q=정보&limit=10
  @GetMapping("/search")
  public List<CertificateSummaryDto> search(
      @RequestParam String q,
      @RequestParam(defaultValue = "10") int limit
  ) {
    return service.search(q, limit);
  }
}