package com.OhRyue.certpilot.versus.config;

import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.web.SecurityFilterChain;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
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

    /**
     * account-service / study-service / cert / community / progress 와
     * 동일한 jwt.secret-key 를 사용합니다.
     *
     * application.yml 예시:
     *
     * jwt:
     *   secret-key: ${JWT_SECRET_KEY:your-super-secure-secret-key123456}
     */
    @Value("${jwt.secret-key:}")
    private String jwtSecret;

    @PostConstruct
    void logSecretLength() {
        if (jwtSecret != null && !jwtSecret.isBlank()) {
            int len = jwtSecret.getBytes(StandardCharsets.UTF_8).length;
            System.out.println("[versus-service] jwt.secret-key length = " + len + " bytes");
        } else {
            System.out.println("[versus-service] WARNING: jwt.secret-key is blank");
        }
    }

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {

        http
                // JWT 기반이므로 CSRF 비활성화
                .csrf(csrf -> csrf.disable())
                // 세션 미사용 (STATELESS)
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                // 인가 정책
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(SWAGGER).permitAll()
                        .requestMatchers(ACTUATOR).permitAll()
                        .requestMatchers("/actuator/**").permitAll()
                        // 대전/토너먼트/골든벨 API 는 전부 로그인 사용자 기준이므로 인증 필수
                        .requestMatchers("/api/**").authenticated()
                        .anyRequest().permitAll()
                )
                // Resource Server - JWT 사용
                .oauth2ResourceServer(oauth -> oauth.jwt(Customizer.withDefaults()))
                // 폼 로그인 / HTTP Basic 비활성
                .httpBasic(httpBasic -> httpBasic.disable())
                .formLogin(form -> form.disable());

        return http.build();
    }

    @Bean
    public JwtDecoder jwtDecoder() {
        if (jwtSecret == null || jwtSecret.isBlank()) {
            throw new IllegalStateException("versus-service: jwt.secret-key must be configured.");
        }
        SecretKey key = new SecretKeySpec(jwtSecret.getBytes(StandardCharsets.UTF_8), "HmacSHA256");
        return NimbusJwtDecoder.withSecretKey(key).build();
    }
}
