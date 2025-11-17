package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.CertDtos.TopCertListResponse;
import com.OhRyue.certpilot.cert.service.CertQueryService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
    private final ObjectMapper objectMapper;

    private String toJson(Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("JSON 직렬화 실패", e);
        }
    }

    /**
     * 온보딩: 프로필 설정 화면에서 보여줄 자격증 Top4 목록
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
        TopCertListResponse response = certQueryService.getTop4Certs(); // 서비스에서 값 생성
        return toJson(response);                                        // JSON 문자열로 감싸서 반환
    }

    @Operation(summary = "Cert 서비스 헬스 체크")
    @GetMapping("/ping")
    public String ping() {
        // 간단한 건 직접 JSON 문자열 하드코딩
        return "{\"service\":\"cert\",\"ok\":true}";
    }
}
