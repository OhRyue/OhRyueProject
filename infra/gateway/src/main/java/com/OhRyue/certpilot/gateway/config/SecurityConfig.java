package com.OhRyue.certpilot.gateway.config;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.gateway.filter.ratelimit.KeyResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.jwt.NimbusReactiveJwtDecoder;
import org.springframework.security.oauth2.jwt.ReactiveJwtDecoder;
import org.springframework.security.web.server.SecurityWebFilterChain;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

@Configuration
@EnableWebFluxSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  /** RateLimitConfig의 Bean을 주입만 받습니다(여기서는 @Bean 정의 금지). */
  private final KeyResolver userKeyResolver;

  /** account-service의 jwt.secret-key(Base64) 사용 */
  @Bean
  public ReactiveJwtDecoder jwtDecoder(@Value("${security.jwt.hmac-secret}") String secret) {
    if (secret == null || secret.isBlank()) {
      throw new IllegalStateException("GATEWAY security.jwt.hmac-secret is not configured.");
    }
    byte[] keyBytes = decodeSecret(secret);
    SecretKey key = new SecretKeySpec(keyBytes, "HmacSHA256");
    return NimbusReactiveJwtDecoder.withSecretKey(key).build();
  }

  @Bean
  public SecurityWebFilterChain springSecurity(ServerHttpSecurity http) {
    http
        // CORS는 전부 application.yml의 spring.cloud.gateway.globalcors 에서만 처리
        // .cors(...) 자체를 호출하지 않습니다.
        .csrf(ServerHttpSecurity.CsrfSpec::disable)
        .authorizeExchange(reg -> reg
            .pathMatchers("/actuator/**").permitAll()
            // 지금은 인증 강제하지 않고, 유효 토큰이 있으면 Principal만 채우는 모드
            .anyExchange().permitAll()
        )
        .oauth2ResourceServer(o -> o.jwt(j -> {
          // 기본 서명 검증만 사용 (추가 커스터마이징 필요 시 여기서)
        }));

    return http.build();
  }

  private byte[] decodeSecret(String secret) {
    try {
      return Base64.getDecoder().decode(secret);
    } catch (IllegalArgumentException ex) {
      return secret.getBytes(StandardCharsets.UTF_8);
    }
  }
}
