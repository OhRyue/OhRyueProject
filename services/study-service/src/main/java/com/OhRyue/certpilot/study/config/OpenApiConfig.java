package com.OhRyue.certpilot.study.config;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OpenApiConfig {
  @Bean
  public OpenAPI studyOpenAPI() {
    return new OpenAPI()
        .info(new Info()
            .title("CertPilot - Study Service API")
            .version("v1")
            .description("개념/미니체크/객관식/실기/보조학습/리포트 API"))
        .externalDocs(new ExternalDocumentation()
            .description("Gateway: /api/study/**"));
  }
}
