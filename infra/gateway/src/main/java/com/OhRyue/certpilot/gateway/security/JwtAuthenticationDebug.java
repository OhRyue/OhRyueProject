package com.OhRyue.certpilot.gateway.security;

import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Base64;

@Slf4j
@Component
public class JwtAuthenticationDebug {

  @Value("${auth.jwt.secret}")
  private String secret;

  @PostConstruct
  public void init() {
    if (secret == null) {
      log.warn("⚠️ [GATEWAY] auth.jwt.secret is NULL");
      return;
    }

    log.info("🚀 [GATEWAY] auth.jwt.secret length = {} chars", secret.length());

    try {
      byte[] decoded = Base64.getDecoder().decode(secret);
      log.info("🚀 [GATEWAY] Base64 decoded length = {} bytes", decoded.length);
    } catch (Exception e) {
      log.warn("⚠️ [GATEWAY] Base64 decode failed → using raw only ({} chars)", secret.length());
    }
  }
}
