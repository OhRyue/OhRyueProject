package com.OhRyue.certpilot.calendar.client;

import com.OhRyue.certpilot.calendar.dto.CertNameDto;
import org.springframework.stereotype.Component;

@Component
public class CertsClientFallback implements CertsClient {

  @Override
  public CertNameDto getById(Long id) {
    // Fallback: return unknown cert name
    return new CertNameDto(id, "자격증 정보 없음", null);
  }
}

