package com.OhRyue.certpilot.cert.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    // Swagger & Actuator 경로 화이트리스트
    private static final String[] SWAGGER_WHITELIST = {
            "/swagger-ui.html",
            "/swagger-ui/**",
            "/v3/api-docs/**"
    };

    private static final String[] ACTUATOR_WHITELIST = {
            "/actuator/health",
            "/actuator/info"
    };

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {

        http
                // CSRF 비활성화 (API 서버, JWT 기반 전제로 개발 단계에서는 꺼둠)
                .csrf(csrf -> csrf.disable())

                // 세션은 사용하지 않음 (Stateless)
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )

                // 요청별 인가 규칙
                .authorizeHttpRequests(auth -> auth
                        // Swagger 문서
                        .requestMatchers(SWAGGER_WHITELIST).permitAll()
                        // 기본 health/info
                        .requestMatchers(ACTUATOR_WHITELIST).permitAll()
                        // 나머지 actuator도 개발 단계에서는 열어둠
                        .requestMatchers("/actuator/**").permitAll()
                        // 모든 API 오픈 (운영 전까지)
                        .requestMatchers("/api/**").permitAll()
                        // 그 외 모든 요청도 일단 허용
                        .anyRequest().permitAll()
                )

                // 기본 로그인 폼 / HTTP Basic 인증 비활성화
                .httpBasic(httpBasic -> httpBasic.disable())
                .formLogin(form -> form.disable());

        return http.build();
    }
}
