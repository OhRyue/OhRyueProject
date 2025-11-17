package com.OhRyue.certpilot.study.config;

import com.OhRyue.certpilot.study.security.RateLimitFilter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.web.SecurityFilterChain;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;

@Configuration
@EnableMethodSecurity // @PreAuthorize 사용 가능
public class SecurityConfig {

  @Value("${jwt.secret-key:}")
  private String hmacSecret;

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
    // Stateless API
    http.csrf(csrf -> csrf.disable());

    http.authorizeHttpRequests(reg -> reg
        // Swagger & actuator 일부 오픈(필요 시 조정)
        .requestMatchers(
            "/swagger-ui/**", "/swagger-ui.html", "/v3/api-docs/**",
            "/actuator/health", "/actuator/info").permitAll()
        // 학습 API는 인증 필요
        .requestMatchers("/api/study/**").authenticated()
        .anyRequest().permitAll()
    );

    http.oauth2ResourceServer(oauth2 ->
        oauth2.jwt(Customizer.withDefaults())
    );

    return http.build();
  }

  @Bean
  public JwtDecoder jwtDecoder() {
    if (hmacSecret == null || hmacSecret.isBlank()) {
      throw new IllegalStateException("JWT 시크릿이 설정되지 않았습니다. jwt.secret-key (환경변수 JWT_SECRET_KEY)를 설정하세요.");
    }
    SecretKey key = new SecretKeySpec(hmacSecret.getBytes(StandardCharsets.UTF_8), "HmacSHA256");
    return NimbusJwtDecoder.withSecretKey(key).build();
  }

  /** 간단 인메모리 레이트리밋 필터 등록 (운영 전 Redis/게이트웨이로 대체 권장) */
  @Bean
  public FilterRegistrationBean<RateLimitFilter> rateLimitFilterRegistration() {
    FilterRegistrationBean<RateLimitFilter> reg = new FilterRegistrationBean<>();
    reg.setFilter(new RateLimitFilter());
    reg.addUrlPatterns("/api/study/*"); // 적용 경로
    reg.setOrder(1); // 보안필터 전에/후에 배치가 필요하면 조정
    return reg;
  }
}
