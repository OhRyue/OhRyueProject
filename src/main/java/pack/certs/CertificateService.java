package pack.certs;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import pack.common.error.NotFoundException;
import pack.certs.dto.CertificateDetailDto;
import pack.certs.dto.CertificateSummaryDto;

import java.util.List;

@Service
public class CertificateService {

  private final CertificateRepository repo;

  public CertificateService(CertificateRepository repo) {
    this.repo = repo;
  }

  public Page<CertificateSummaryDto> findAll(String field, Pageable pageable) {
    Page<Certificate> page;
    if (field != null && !field.isBlank()) {
      page = repo.findByFieldIgnoreCase(field.trim(), pageable);
    } else {
      page = repo.findAll(pageable);
    }
    return page.map(CertificateSummaryDto::from);
  }

  public List<CertificateSummaryDto> search(String q, int limit) {
    if (q == null || q.isBlank()) return List.of();
    if (limit <= 0) limit = 10;
    // 간단 구현: Top10 고정 + 필요 시 stream limit
    return repo.findTop10ByNameContainingIgnoreCase(q.trim())
        .stream()
        .limit(limit)
        .map(CertificateSummaryDto::from)
        .toList();
  }

  public CertificateDetailDto getById(Long id) {
    Certificate c = repo.findById(id)
        .orElseThrow(() -> new NotFoundException("certificate not found: " + id));
    return CertificateDetailDto.from(c);
  }
}
