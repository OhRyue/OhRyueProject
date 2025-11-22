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
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.reactive.CorsConfigurationSource;
import org.springframework.web.cors.reactive.UrlBasedCorsConfigurationSource;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
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
    return http
        .cors(cors -> cors.configurationSource(corsConfigurationSource()))
        .csrf(ServerHttpSecurity.CsrfSpec::disable)
        .authorizeExchange(reg -> reg
            .pathMatchers("/actuator/**").permitAll()
            .anyExchange().permitAll() // 인증 강제 X (유효 토큰 시 Principal 채움)
        )
        .oauth2ResourceServer(o -> o.jwt(j -> { /* 기본 서명검증 */ }))
        .build();
  }

  @Bean
  public CorsConfigurationSource corsConfigurationSource() {
    CorsConfiguration config = new CorsConfiguration();
    config.setAllowedOrigins(Arrays.asList(
        "http://localhost:3000",
        "https://mycertpilot.com",
        "https://www.mycertpilot.com"
    ));
    config.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"));
    config.setAllowedHeaders(Arrays.asList("*"));
    config.setAllowCredentials(true);
    config.setMaxAge(3600L);

    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    source.registerCorsConfiguration("/**", config);
    return source;
  }

  private byte[] decodeSecret(String secret) {
    try {
      return Base64.getDecoder().decode(secret);
    } catch (IllegalArgumentException ex) {
      return secret.getBytes(StandardCharsets.UTF_8);
    }
  }
}
