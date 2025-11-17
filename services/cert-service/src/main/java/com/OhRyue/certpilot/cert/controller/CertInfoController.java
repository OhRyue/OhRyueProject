package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.api.CertInfoDto;
import com.OhRyue.certpilot.cert.service.CertInfoService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Cert - Info", description = "자격증 정보 조회")
@RestController
@RequestMapping(
        value = "/api/cert",
        produces = MediaType.APPLICATION_JSON_VALUE
)
@RequiredArgsConstructor
public class CertInfoController {

    private final CertInfoService certInfoService;
    private final ObjectMapper objectMapper;

    private String toJson(Object value) {
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            throw new RuntimeException("JSON 직렬화 실패", e);
        }
    }

    @Operation(summary = "사용자의 목표 자격증 상세 정보 조회")
    @GetMapping("/current")
    public ResponseEntity<String> current(Authentication authentication) {
        // Swagger에서 userId 입력받지 않고, Authorization 토큰에서 바로 사용
        if (authentication == null || authentication.getName() == null) {
            return ResponseEntity.noContent().build();
        }
        String userId = authentication.getName();

        return certInfoService.currentCert(userId)
                .map(dto -> ResponseEntity.ok(toJson(dto)))
                .orElseGet(() -> ResponseEntity.noContent().build());
    }

    @Operation(summary = "큐넷 기반 학습 팁 조회")
    @GetMapping("/{jmCd}/tips")
    public ResponseEntity<String> tips(@PathVariable String jmCd) {
        return certInfoService.tips(jmCd)
                .map(dto -> ResponseEntity.ok(toJson(dto)))
                .orElseGet(() -> ResponseEntity.noContent().build());
    }
}
