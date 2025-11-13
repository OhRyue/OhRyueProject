package com.OhRyue.certpilot.cert.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@Tag(name = "Cert - Ping", description = "자격증 서비스 상태 확인")
@RestController
@RequestMapping("/cert")
public class CertController {

  @Operation(summary = "Cert 서비스 헬스 체크")
  @GetMapping("/ping")
  public Map<String, Object> ping() {
    return Map.of("service", "cert", "ok", true);
  }
}