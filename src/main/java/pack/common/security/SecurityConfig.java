package pack.common.security;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
public class SecurityConfig {

  @Bean
  public SecurityFilterChain api(HttpSecurity http) throws Exception {
    http
        .cors(Customizer.withDefaults()) // ★ WebMvc CORS or CorsConfigurationSource 사용
        .csrf(csrf -> csrf.disable())    // API 서버면 보통 비활성(토큰 기반)
        .authorizeHttpRequests(auth -> auth
            // preflight(OPTIONS) 모두 허용
            .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
            // swagger & api-docs 개발 중 열기
            .requestMatchers(
                "/v3/api-docs/**",
                "/swagger-ui/**",
                "/swagger-ui.html"
            ).permitAll()
            .anyRequest().permitAll() // 필요 시 조정 (인증 붙이면 변경)
        );
    return http.build();
  }
}
