package pack.common.web;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {

  // 프론트 Vite(기본 5173)와 연동 용 CORS
  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry.addMapping("/**")
        .allowedOrigins(
            "http://localhost:5173",
            "http://127.0.0.1:5173"
        )
        .allowedMethods("GET","POST","PUT","DELETE","PATCH","OPTIONS")
        .allowCredentials(true);
  }
}
