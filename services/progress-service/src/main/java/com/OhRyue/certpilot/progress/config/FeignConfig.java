package com.OhRyue.certpilot.progress.config;

import feign.Logger;
import feign.RequestInterceptor;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

import javax.crypto.SecretKey;
import java.util.Date;
import java.util.List;

@Slf4j
@Configuration
public class FeignConfig {

  @Bean
  Logger.Level feignLoggerLevel() {
    // FULL은 민감정보 노출/로그 폭발 위험이 커서 BASIC 권장
    return Logger.Level.BASIC;
  }

  @Bean
  public RequestInterceptor requestInterceptor(
      @Value("${auth.internal.jwt.secret:}") String internalSecret,
      @Value("${auth.internal.jwt.issuer:certpilot-internal}") String issuer,
      @Value("${auth.internal.jwt.expiration-minutes:15}") int expirationMinutes
  ) {
    if (!StringUtils.hasText(internalSecret)) {
      throw new IllegalArgumentException("auth.internal.jwt.secret (INTERNAL_JWT_SECRET) must be set");
    }

    // internal secret은 base64일 수도/아닐 수도 있으니 decode 시도
    SecretKey secretKey = Keys.hmacShaKeyFor(decodeSecret(internalSecret));
    int expMin = expirationMinutes > 0 ? expirationMinutes : 15;

    return template -> {
      try {
        String path = template.url(); // Feign template url (상대경로가 대부분)

        // 내부 호출: /api/study/internal/** 또는 /api/versus/internal/**
        boolean isStudyInternal = path != null && path.startsWith("/sessions/"); // StudyInternalClient의 base url이 /api/study/internal 이라서 path는 /sessions/... 로 옴
        boolean isVersusInternal = path != null && path.startsWith("/matches/"); // VersusInternalClient base url이 /api/versus/internal

        if (isStudyInternal || isVersusInternal) {
          String audience = isStudyInternal ? "study-service" : "versus-service";
          String internalToken = generateInternalToken(secretKey, issuer, expMin, audience);

          template.header("Authorization"); // clear
          template.header("Authorization", "Bearer " + internalToken);

          log.info("[progress-service] ✅ Feign internal call -> attach INTERNAL JWT. audience={}, path={}", audience, path);
          return;
        }

        // ===== 그 외: 사용자 JWT 전달 (기존 로직 유지) =====
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (attributes != null) {
          HttpServletRequest req = attributes.getRequest();
          String authorization = req.getHeader("Authorization");
          if (StringUtils.hasText(authorization)) {
            template.header("Authorization");
            template.header("Authorization", authorization);
          }
        }
      } catch (Exception e) {
        log.error("[progress-service] Feign interceptor error", e);
      }
    };
  }

  private static String generateInternalToken(SecretKey secretKey, String issuer, int expirationMinutes, String audience) {
    long now = System.currentTimeMillis();
    Date issuedAt = new Date(now);
    Date expiration = new Date(now + expirationMinutes * 60L * 1000);

    return Jwts.builder()
        .setSubject("internal-progress-service")
        .setIssuer(issuer)
        .setAudience(audience)
        .claim("roles", List.of("INTERNAL"))
        .setIssuedAt(issuedAt)
        .setExpiration(expiration)
        .signWith(secretKey, SignatureAlgorithm.HS256)
        .compact();
  }

  private static byte[] decodeSecret(String secret) {
    try {
      return Decoders.BASE64.decode(secret);
    } catch (Exception e) {
      return secret.getBytes();
    }
  }
}
