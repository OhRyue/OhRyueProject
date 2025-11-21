package com.OhRyue.certpilot.cert.config;

import com.OhRyue.certpilot.cert.security.JwtAuthFilter;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity // @PreAuthorize 사용 가능
@RequiredArgsConstructor
public class SecurityConfig {

    private static final String[] SWAGGER_WHITELIST = {
            "/swagger-ui.html",
            "/swagger-ui/**",
            "/v3/api-docs/**"
    };

    private static final String[] ACTUATOR_WHITELIST = {
            "/actuator/health",
            "/actuator/info"
    };

    private final JwtAuthFilter jwtAuthFilter;

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {

        http
                // CSRF 비활성화 (API 서버, JWT 기반)
                .csrf(csrf -> csrf.disable())

                // 세션은 사용하지 않음 (Stateless)
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )

                // 인증 실패 처리
                .exceptionHandling(ex ->
                        ex.authenticationEntryPoint((request, response, authException) -> {
                            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "인증이 필요합니다");
                        })
                )

                // 요청별 인가 규칙
                .authorizeHttpRequests(auth -> auth
                        // Swagger 문서
                        .requestMatchers(SWAGGER_WHITELIST).permitAll()
                        // health/info
                        .requestMatchers(ACTUATOR_WHITELIST).permitAll()
                        // 개발 단계에서 다른 actuator도 열어두고 싶다면:
                        // .requestMatchers("/actuator/**").permitAll()

                        // 실제 Cert API (자격증 정보/토픽 등)는 JWT 필요
                        .requestMatchers("/api/cert/**").authenticated()

                        // 그 외 요청은 일단 허용
                        .anyRequest().permitAll()
                )

                // Resource Server 방식 대신, 커스텀 JwtAuthFilter 사용
                .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class)

                // 기본 로그인 폼 / HTTP Basic 인증 비활성화
                .httpBasic(httpBasic -> httpBasic.disable())
                .formLogin(form -> form.disable());

        return http.build();
    }
}
