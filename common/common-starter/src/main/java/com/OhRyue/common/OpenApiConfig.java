package com.OhRyue.common;


import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OpenApiConfig {

  @Bean
  public OpenAPI openAPI() {
    return new OpenAPI()
        .info(new Info().title("OhRyueProject API").version("v1"))
        .externalDocs(new ExternalDocumentation().description("Docs"));
  }

  @Bean
  public GroupedOpenApi groupedOpenApi(@Value("${spring.application.name:api}") String appName) {
    String groupName = appName.replace("-service", "-api");
    return GroupedOpenApi.builder()
        .group(groupName)
        .pathsToMatch("/api/**")
        .build();
  }
}