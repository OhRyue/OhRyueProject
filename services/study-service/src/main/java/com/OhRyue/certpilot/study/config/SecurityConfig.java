package com.OhRyue.certpilot.study.config;

import com.OhRyue.certpilot.study.security.JwtAuthFilter;
import com.OhRyue.certpilot.study.security.RateLimitFilter;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableMethodSecurity // @PreAuthorize 사용 가능
@RequiredArgsConstructor
public class SecurityConfig {

  private final JwtAuthFilter jwtAuthFilter;

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
    http
        // CORS는 Gateway에서만 처리
        .cors(AbstractHttpConfigurer::disable)
        // CSRF 비활성화
        .csrf(AbstractHttpConfigurer::disable)
        // 세션 Stateless
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
        )
        // 인증 실패 처리
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((request, response, authException) -> {
              response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "인증이 필요합니다");
            })
        )
        // 인가 규칙
        .authorizeHttpRequests(reg -> reg
            // Swagger & actuator 일부 오픈
            .requestMatchers(
                "/swagger-ui/**",
                "/swagger-ui.html",
                "/v3/api-docs/**",
                "/actuator/health",
                "/actuator/info"
            ).permitAll()
            // (옵션) 프리플라이트 직접 받을 경우
            .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
            // 학습 API는 인증 필요
            .requestMatchers("/api/study/**").authenticated()
            // 그 외는 일단 허용
            .anyRequest().permitAll()
        )
        // JWT 필터를 UsernamePasswordAuthenticationFilter 앞에 배치
        .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }

  /**
   * 간단 인메모리 레이트리밋 필터 등록 (운영 전 Redis/게이트웨이로 대체 권장)
   */
  @Bean
  public FilterRegistrationBean<RateLimitFilter> rateLimitFilterRegistration() {
    FilterRegistrationBean<RateLimitFilter> reg = new FilterRegistrationBean<>();
    reg.setFilter(new RateLimitFilter());
    reg.addUrlPatterns("/api/study/*"); // 적용 경로
    reg.setOrder(1); // 보안 필터 체인 앞/뒤 조정 필요하면 숫자 변경
    return reg;
  }
}
