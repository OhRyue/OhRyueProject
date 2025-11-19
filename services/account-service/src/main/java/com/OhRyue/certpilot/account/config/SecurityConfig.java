package com.OhRyue.certpilot.account.config;

import com.OhRyue.certpilot.account.security.JwtAuthenticationFilter;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.List;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  private final JwtAuthenticationFilter jwtAuthenticationFilter;

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
        // CORS 설정
        .cors(cors -> cors.configurationSource(corsConfigurationSource()))
        // CSRF 비활성화 (JWT / API 서버)
        .csrf(csrf -> csrf.disable())
        // 세션 사용 안 함 (Stateless)
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
        )
        // 인증 실패 처리
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((request, response, authException) -> {
              response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "로그인이 필요합니다");
            })
        )
        // 인가 설정
        .authorizeHttpRequests(auth -> auth
            // Actuator 화이트리스트: Prometheus/헬스체크는 인증 없이 허용
            .requestMatchers(
                "/actuator/health",
                "/actuator/info",
                "/actuator/prometheus"
            ).permitAll()

            // 회원가입/로그인/이메일 인증 등 공개 API
            .requestMatchers(
                "/api/account/send-verification",
                "/api/account/verify-email",
                "/api/account/login",
                "/api/account/check-userId",
                "/api/account/forgot-password",
                "/api/account/forgot-password/verify",
                "/api/account/forgot-password/reset",
                "/api/account/refresh",
                "/api/mail/**",
                "/v3/api-docs/**",
                "/swagger-ui/**",
                "/swagger-ui.html"
            ).permitAll()

            // 그 외 모든 요청은 인증 필요
            .anyRequest().authenticated()
        )
        // JWT 필터를 UsernamePasswordAuthenticationFilter 앞에 배치
        .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }

  @Bean
  public CorsConfigurationSource corsConfigurationSource() {
    CorsConfiguration config = new CorsConfiguration();
    config.setAllowedOrigins(List.of("http://localhost:3000"));
    config.setAllowedMethods(List.of("GET", "POST", "PUT", "DELETE", "OPTIONS"));
    config.setAllowedHeaders(List.of("*"));
    config.setAllowCredentials(true);

    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    source.registerCorsConfiguration("/**", config);
    return source;
  }

  @Bean
  public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
    return config.getAuthenticationManager();
  }
}
