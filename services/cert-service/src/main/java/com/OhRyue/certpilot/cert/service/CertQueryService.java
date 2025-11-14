package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.CertEntity;
import com.OhRyue.certpilot.cert.domain.CertEntity;
import com.OhRyue.certpilot.cert.dto.CertDtos.CertSummaryResponse;
import com.OhRyue.certpilot.cert.dto.CertDtos.TopCertListResponse;
import com.OhRyue.certpilot.cert.repository.CertRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class CertQueryService {

    private final CertRepository certRepository;

    /**
     * 온보딩에서 사용할 Top4 자격증 목록 조회
     * - 현재는 단순 id 오름차순 상위 4개
     */
    public TopCertListResponse getTop4Certs() {
        List<CertEntity> certs = certRepository.findTop4ByOrderByIdAsc();

        List<CertSummaryResponse> summaries = certs.stream()
                .map(CertSummaryResponse::from)
                .toList();

        log.debug("Loaded top4 certs for onboarding, count={}", summaries.size());

        return TopCertListResponse.of(summaries);
    }
}
