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
        log.info("🚀 [GATEWAY] Loaded auth.jwt.secret (raw) = {}", secret);

        try {
            byte[] decoded = Base64.getDecoder().decode(secret);
            log.info("🚀 [GATEWAY] Base64 decoded length = {} bytes", decoded.length);
        } catch (Exception e) {
            log.warn("⚠️ [GATEWAY] Base64 decode failed → using raw only");
        }
    }
}
