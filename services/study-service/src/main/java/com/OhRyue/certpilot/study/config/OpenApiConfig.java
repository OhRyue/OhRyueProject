package com.OhRyue.certpilot.study.config;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Info;
import org.springframework.context.annotation.Configuration;

@OpenAPIDefinition(
    info = @Info(
        title = "Study Service API",
        version = "1.0.0",
        description = "메인학습·보조학습·실기 채점 API"
    )
)
@Configuration
public class OpenApiConfig {}
