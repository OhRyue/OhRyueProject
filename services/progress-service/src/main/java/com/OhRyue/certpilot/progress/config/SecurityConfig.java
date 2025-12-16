package com.OhRyue.certpilot.progress.config;

import com.OhRyue.certpilot.progress.security.InternalJwtAuthFilter;
import com.OhRyue.certpilot.progress.security.JwtAuthFilter;
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
            "/swagger-ui/**", "/swagger-ui.html", "/v3/api-docs/**"
    };

    private static final String[] ACTUATOR = {
            "/actuator/health", "/actuator/info", "/actuator/prometheus"
    };

    private final InternalJwtAuthFilter internalJwtAuthFilter;
    private final JwtAuthFilter jwtAuthFilter;

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .cors(AbstractHttpConfigurer::disable)
                .csrf(AbstractHttpConfigurer::disable)
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .exceptionHandling(ex ->
                        ex.authenticationEntryPoint((request, response, authException) ->
                                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "인증이 필요합니다")
                        )
                )
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(SWAGGER).permitAll()
                        .requestMatchers(ACTUATOR).permitAll()
                        .requestMatchers("/actuator/**").permitAll()

                        // 내부 전용: Versus 결과/보상 지급은 INTERNAL만
                        .requestMatchers("/api/progress/versus/**").hasRole("INTERNAL")

                        // (정책에 따라 나중에 여기도 INTERNAL로 강화 가능)
                        .requestMatchers("/api/progress/internal/**").permitAll()
                        .requestMatchers("/api/progress/notifications/create").permitAll()

                        // 나머지 progress는 로그인 사용자 JWT
                        .requestMatchers("/api/progress/**").authenticated()
                        .anyRequest().permitAll()
                )
                // 내부 필터 먼저 (versus 경로에서 ROLE_INTERNAL 세팅)
                .addFilterBefore(internalJwtAuthFilter, UsernamePasswordAuthenticationFilter.class)
                // 그 다음 유저 JWT
                .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class)
                .httpBasic(AbstractHttpConfigurer::disable)
                .formLogin(AbstractHttpConfigurer::disable);

        return http.build();
    }
}
