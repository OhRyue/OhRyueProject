package com.OhRyue.certpilot.versus.config;

import com.OhRyue.certpilot.versus.security.JwtAuthFilter;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  private static final String[] SWAGGER = {
      "/swagger-ui/**",
      "/swagger-ui.html",
      "/v3/api-docs/**"
  };

  private static final String[] ACTUATOR = {
      "/actuator/health",
      "/actuator/info",
      "/actuator/prometheus"
  };

  // WebSocket 엔드포인트 (HandshakeInterceptor에서 JWT 인증 처리)
  private static final String[] WEBSOCKET = {
      "/ws/versus/**",
      "/ws/versus"
  };

  private final JwtAuthFilter jwtAuthFilter;

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {

    http
        // CORS는 Gateway에서만 처리
        .cors(AbstractHttpConfigurer::disable)
        .csrf(AbstractHttpConfigurer::disable)
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
        )
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((request, response, authException) -> {
              response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "인증이 필요합니다");
            })
        )
        .authorizeHttpRequests(auth -> auth
            .requestMatchers(SWAGGER).permitAll()
            .requestMatchers(ACTUATOR).permitAll()
            .requestMatchers("/actuator/**").permitAll()
            // WebSocket 엔드포인트는 HandshakeInterceptor에서 JWT 인증 처리
            // Security Filter Chain에서는 허용 (인증은 Handshake 단계에서 수행)
            .requestMatchers(WEBSOCKET).permitAll()
            // 대전/토너먼트/골든벨 API 는 전부 로그인 사용자 기준이므로 인증 필수
            .requestMatchers("/api/versus/**").authenticated()
            .anyRequest().permitAll()
        )
        .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class)
        .httpBasic(AbstractHttpConfigurer::disable)
        .formLogin(AbstractHttpConfigurer::disable);

    return http.build();
  }
}
