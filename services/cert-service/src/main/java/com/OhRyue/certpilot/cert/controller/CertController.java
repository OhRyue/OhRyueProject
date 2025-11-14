package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.CertDtos.TopCertListResponse;
import com.OhRyue.certpilot.cert.service.CertQueryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/api/cert")
@RequiredArgsConstructor
@Tag(
        name = "Cert API",
        description = "자격증 기본정보/온보딩용 자격증 목록 조회 API"
)
public class CertController {
    private final CertQueryService certQueryService;

    /**
     * 온보딩: 프로필 설정 화면에서 보여줄 자격증 Top4 목록
     *
     * - 프론트는 이 API로 받은 목록을 버튼 4개로 렌더링
     * - 사용자가 버튼을 클릭하면 선택된 certId를
     *   /api/account/onboarding/profile 로 그대로 넘기면 됩니다.
     *
     * 예시 응답:
     * {
     *   "certs": [
     *     { "id": 1, "name": "정보처리기사", ... },
     *     { "id": 2, "name": "컴퓨터활용능력 2급", ... },
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
    public TopCertListResponse getTop4Certs() {
        return certQueryService.getTop4Certs();
    }

    @Operation(summary = "Cert 서비스 헬스 체크")
    @GetMapping("/ping")
    public Map<String, Object> ping() {
    return Map.of("service", "cert", "ok", true);
  }
}