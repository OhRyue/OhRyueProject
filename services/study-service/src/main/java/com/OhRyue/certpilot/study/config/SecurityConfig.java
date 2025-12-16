package com.OhRyue.certpilot.study.config;

import com.OhRyue.certpilot.study.security.InternalJwtAuthFilter;
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
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  private final JwtAuthFilter jwtAuthFilter;
  private final InternalJwtAuthFilter internalJwtAuthFilter;

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
    http
        .cors(AbstractHttpConfigurer::disable)
        .csrf(AbstractHttpConfigurer::disable)
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS)
        )
        .exceptionHandling(ex ->
            ex.authenticationEntryPoint((req, res, e) ->
                res.sendError(HttpServletResponse.SC_UNAUTHORIZED, "인증이 필요합니다")
            )
        )
        .authorizeHttpRequests(reg -> reg
            .requestMatchers(
                "/swagger-ui/**",
                "/swagger-ui.html",
                "/v3/api-docs/**",
                "/actuator/health",
                "/actuator/info"
            ).permitAll()
            .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()

            // 내부 호출 전용
            .requestMatchers("/api/study/versus/**").hasRole("INTERNAL")

            // 일반 사용자
            .requestMatchers("/api/study/**").authenticated()
            .anyRequest().permitAll()
        )
        .addFilterBefore(internalJwtAuthFilter, UsernamePasswordAuthenticationFilter.class)
        .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class);

    return http.build();
  }

  @Bean
  public FilterRegistrationBean<RateLimitFilter> rateLimitFilterRegistration() {
    FilterRegistrationBean<RateLimitFilter> reg = new FilterRegistrationBean<>();
    reg.setFilter(new RateLimitFilter());
    reg.addUrlPatterns("/api/study/*");
    reg.setOrder(1);
    return reg;
  }
}
