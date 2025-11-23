package com.OhRyue.certpilot.account.config;

import com.OhRyue.certpilot.account.security.JwtAuthFilter;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  private final JwtAuthFilter jwtAuthFilter;

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  /**
   * 여기서는 CORS 를 완전히 비활성화합니다.
   * - 브라우저에서 보이는 Access-Control-XXX 헤더는 오직 Gateway 가 책임지도록 합니다.
   */
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
        // CORS는 gateway에서만 처리하도록 완전히 disable
        .cors(AbstractHttpConfigurer::disable)
        .csrf(AbstractHttpConfigurer::disable)
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
        )
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((request, response, authException) ->
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "로그인이 필요합니다")
            )
        )
        .authorizeHttpRequests(auth -> auth
            // CORS preflight OPTIONS 요청 허용
            .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
            // Swagger / Actuator
            .requestMatchers(
                "/v3/api-docs/**",
                "/swagger-ui/**",
                "/swagger-ui.html",
                "/actuator/health",
                "/actuator/info",
                "/actuator/prometheus"
            ).permitAll()
            // 회원가입/로그인/비번찾기 등 공개 API
            .requestMatchers(
                "/api/account/send-verification",
                "/api/account/verify-email",
                "/api/account/login",
                "/api/account/check-userId",
                "/api/account/forgot-password",
                "/api/account/forgot-password/verify",
                "/api/account/forgot-password/reset",
                "/api/account/refresh",
                "/api/mail/**"
            ).permitAll()
            // 그 외는 모두 인증 필요
            .anyRequest().authenticated()
        )
        .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }

  @Bean
  public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
    return config.getAuthenticationManager();
  }
}
