package com.OhRyue.certpilot.community.config;

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
            "/swagger-ui/**", "/swagger-ui.html", "/v3/api-docs/**"
    };

    private static final String[] ACTUATOR = {
            "/actuator/health", "/actuator/info"
    };

    @Value("${jwt.secret-key:}")
    private String jwtSecret;

    @PostConstruct
    void logSecretLength() {
        if (jwtSecret != null && !jwtSecret.isBlank()) {
            int len = jwtSecret.getBytes(StandardCharsets.UTF_8).length;
            System.out.println("[community-service] jwt.secret-key length = " + len + " bytes");
        } else {
            System.out.println("[community-service] WARNING: jwt.secret-key is blank");
        }
    }

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .csrf(csrf -> csrf.disable())
                .sessionManagement(session ->
                        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                )
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(SWAGGER).permitAll()
                        .requestMatchers(ACTUATOR).permitAll()
                        .requestMatchers("/actuator/**").permitAll()
                        // 커뮤니티 API는 사용자 정보가 필요하니 JWT 필수 (향후 @PreAuthorize 사용도 가능)
                        .requestMatchers("/api/**").authenticated()
                        .anyRequest().permitAll()
                )
                // JWT Resource Server
                .oauth2ResourceServer(oauth -> oauth.jwt(Customizer.withDefaults()))
                // 폼 로그인 / Basic 비활성
                .httpBasic(httpBasic -> httpBasic.disable())
                .formLogin(form -> form.disable());

        return http.build();
    }

    @Bean
    public JwtDecoder jwtDecoder() {
        if (jwtSecret == null || jwtSecret.isBlank()) {
            throw new IllegalStateException("community-service: jwt.secret-key must be configured.");
        }
        SecretKey key = new SecretKeySpec(jwtSecret.getBytes(StandardCharsets.UTF_8), "HmacSHA256");
        return NimbusJwtDecoder.withSecretKey(key).build();
    }
}
