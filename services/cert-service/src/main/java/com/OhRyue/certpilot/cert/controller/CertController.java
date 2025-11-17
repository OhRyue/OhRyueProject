package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.CertDtos.CertSummaryResponse;
import com.OhRyue.certpilot.cert.dto.CertDtos.TopCertListResponse;
import com.OhRyue.certpilot.cert.service.CertQueryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping(
        value = "/api/cert",
        produces = MediaType.APPLICATION_JSON_VALUE
)
@RequiredArgsConstructor
@Tag(
        name = "Cert API",
        description = "자격증 기본정보/온보딩용 자격증 목록 조회 API"
)
public class CertController {

    private final CertQueryService certQueryService;

    // CurriculumController와 동일한 escape 유틸
    private String escape(String s) {
        if (s == null) return "";
        return s
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    /**
     * 온보딩: 프로필 설정 화면에서 보여줄 자격증 Top4 목록
     *
     * 응답 예:
     * {
     *   "certs": [
     *     { "id": 1, "name": "...", "level": "...", "issuer": "...", "description": "...", ... },
     *     ...
     *   ]
     * }
     */
    @Operation(
            summary = "온보딩용 Top4 자격증 목록",
            description = """
            회원 온보딩(프로필 설정) 화면에서 보여줄 자격증 Top4 목록을 반환합니다.
            - 현재는 단순히 id 오름차순 상위 4개를 사용합니다.
            - 추후 우선순위 컬럼을 추가하면 정렬 기준을 변경할 수 있습니다.
            """
    )
    @GetMapping("/top4")
    public String getTop4Certs() {
        TopCertListResponse response = certQueryService.getTop4Certs();
        // record 이므로 .certs() 로 리스트 꺼냅니다.
        List<CertSummaryResponse> certs = response.certs();

        StringBuilder sb = new StringBuilder();
        sb.append("{\"certs\":[");
        for (int i = 0; i < certs.size(); i++) {
            CertSummaryResponse c = certs.get(i);
            if (i > 0) sb.append(',');

            sb.append('{')
                    .append("\"id\":").append(c.id()).append(',')
                    .append("\"name\":\"").append(escape(c.name())).append("\",")
                    .append("\"level\":\"").append(escape(c.level())).append("\",")
                    .append("\"issuer\":\"").append(escape(c.issuer())).append("\",");

            // description (nullable)
            if (c.description() == null) {
                sb.append("\"description\":null,");
            } else {
                sb.append("\"description\":\"").append(escape(c.description())).append("\",");
            }

            // writtenFee / practicalFee: Integer → JSON number (nullable)
            if (c.writtenFee() == null) {
                sb.append("\"writtenFee\":null,");
            } else {
                sb.append("\"writtenFee\":").append(c.writtenFee()).append(',');
            }

            if (c.practicalFee() == null) {
                sb.append("\"practicalFee\":null");
            } else {
                sb.append("\"practicalFee\":").append(c.practicalFee());
            }

            sb.append('}');
        }
        sb.append("]}");

        return sb.toString();
    }

    @Operation(summary = "Cert 서비스 헬스 체크")
    @GetMapping("/ping")
    public String ping() {
        // 간단한 JSON 하드코딩
        return "{\"service\":\"cert\",\"ok\":true}";
    }
}
