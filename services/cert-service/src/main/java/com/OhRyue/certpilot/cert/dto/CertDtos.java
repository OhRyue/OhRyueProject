package com.OhRyue.certpilot.cert.dto;

import com.OhRyue.certpilot.cert.domain.CertEntity;

import java.util.List;

public class CertDtos {

    /**
     * 온보딩용 "자격증 선택 버튼"에 필요한 최소 정보
     */
    public record CertSummaryResponse(
            Long id,
            String name,
            String level,
            String issuer,
            String description,
            Integer writtenFee,
            Integer practicalFee
    ) {
        public static CertSummaryResponse from(CertEntity cert) {
            return new CertSummaryResponse(
                    cert.getId(),
                    cert.getName(),
                    cert.getLevel(),
                    cert.getIssuer(),
                    cert.getDescription(),
                    cert.getWrittenFee(),
                    cert.getPracticalFee()
            );
        }
    }

    /**
     * Top4 목록 래핑
     */
    public record TopCertListResponse(
            List<CertSummaryResponse> certs
    ) {
        public static TopCertListResponse of(List<CertSummaryResponse> certs) {
            return new TopCertListResponse(certs);
        }
    }
}
