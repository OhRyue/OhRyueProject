package com.OhRyue.certpilot.cert.controller;

import com.OhRyue.certpilot.cert.dto.api.CertInfoDto;
import com.OhRyue.certpilot.cert.service.CertInfoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Cert - Info", description = "자격증 정보 조회")
@RestController
@RequestMapping("/api/cert")
@RequiredArgsConstructor
public class CertInfoController {

  private final CertInfoService certInfoService;

  @Operation(summary = "사용자의 목표 자격증 상세 정보 조회")
  @GetMapping("/current")
  public ResponseEntity<CertInfoDto.CurrentCertResponse> current(@RequestParam String userId) {
    return certInfoService.currentCert(userId)
        .map(ResponseEntity::ok)
        .orElseGet(() -> ResponseEntity.noContent().build());
  }

  @Operation(summary = "큐넷 기반 학습 팁 조회")
  @GetMapping("/{jmCd}/tips")
  public ResponseEntity<CertInfoDto.TipsResponse> tips(@PathVariable String jmCd) {
    return certInfoService.tips(jmCd)
        .map(ResponseEntity::ok)
        .orElseGet(() -> ResponseEntity.noContent().build());
  }
}

