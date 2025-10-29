package com.OhRyue.certpilot.calendar.client;

import com.OhRyue.certpilot.calendar.dto.CertNameDto;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "certs-service", fallback = CertsClientFallback.class)
public interface CertsClient {

  @GetMapping("/api/certs/{id}")
  CertNameDto getById(@PathVariable Long id);
}

