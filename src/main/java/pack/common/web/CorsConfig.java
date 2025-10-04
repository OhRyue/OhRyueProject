package pack.common.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class CorsConfig {

  @Bean
  public WebMvcConfigurer corsConfigurer() {
    return new WebMvcConfigurer() {
      @Override public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
            .allowedOrigins("http://localhost:5173") // Vite dev
            .allowedMethods("GET","POST","PUT","DELETE","OPTIONS")
            .allowedHeaders("Authorization","Content-Type")
            .allowCredentials(true) // 쿠키 인증 쓸 때만 true, 아니면 false도 OK
            .maxAge(3600);
      }
    };
  }
}
