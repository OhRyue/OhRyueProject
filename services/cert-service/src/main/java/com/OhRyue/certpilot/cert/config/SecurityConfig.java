package com.OhRyue.certpilot.cert.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
public class SecurityConfig {

  private static final String[] SWAGGER = {
          "/swagger-ui/**", "/swagger-ui.html", "/v3/api-docs/**",
          "/actuator/health", "/actuator/info"
  };

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
        .csrf(csrf -> csrf.disable())
        .authorizeHttpRequests(auth -> auth
            .requestMatchers(SWAGGER).permitAll()
            .requestMatchers("/actuator/**").permitAll()
            .anyRequest().permitAll() // 개발 단계: 전부 오픈. (운영 전 인증 정책 적용)
        )
        .httpBasic(Customizer.withDefaults());
    // .oauth2ResourceServer(oauth -> oauth.jwt(Customizer.withDefaults())); // 운영 시 활성화

    return http.build();
  }
}
