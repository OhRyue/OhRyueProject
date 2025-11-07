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
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.List;

@Configuration
@EnableWebSecurity // 스프링 시큐리티 활성화
@RequiredArgsConstructor // final 필드 자동 주입 (JwtAuthenticationFilter)
public class SecurityConfig {

  private final JwtAuthenticationFilter jwtAuthenticationFilter;

  /*
      비밀번호 암호화를 위한 Bean
      - 회원가입 시 비밀번호를 BCrypt로 암호화
   */
  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  /*
      스프링 시큐리티 필터 체인 설정
      - 어떤 요청을 허용하고, 어떤 요청에 인증이 필요한지 결정
      - JWT 필터를 UsernamePasswordAuthenticationFilter 전에 실행
   */
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
        .cors(cors -> cors.configurationSource(corsConfigurationSource()))
        .csrf(csrf -> csrf.disable())
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((request, response, authException) -> {
              response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "로그인이 필요합니다");
            })
        )
        .authorizeHttpRequests(auth -> auth
            .requestMatchers(
                "/api/auth/login",
                "/api/account/register",
                "/api/account/refresh",
                "/api/account/verify-email",
                "/api/mail/**",
                "/v3/api-docs/**",      // Swagger JSON
                "/swagger-ui/**",       // Swagger UI HTML
                "/swagger-ui.html"      // 직접 접근 시
            ).permitAll() // 로그인/회원가입은 누구나 가능
            .anyRequest().authenticated()  // 나머지는 JWT 필요
        )
        .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }

  // CORS 설정 (React 3000/5173 포트 허용)
  @Bean
  public CorsConfigurationSource corsConfigurationSource() {
    CorsConfiguration config = new CorsConfiguration();
    config.setAllowedOrigins(List.of("http://localhost:3000", "http://localhost:5173"));
    config.setAllowedMethods(List.of("GET", "POST", "PUT", "DELETE", "OPTIONS"));
    config.setAllowedHeaders(List.of("*"));
    config.setAllowCredentials(true); // 쿠키/JWT 허용

    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
    source.registerCorsConfiguration("/**", config);
    return source;
  }

  /*
      AuthenticationManager (만약 로그인 인증에 필요하면 사용)
   */
  @Bean
  public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
    return config.getAuthenticationManager();
  }
}
