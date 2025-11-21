package com.OhRyue.certpilot.study.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    private static final String SECURITY_SCHEME_NAME = "bearerAuth";

    @Bean
    public OpenAPI studyOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("CertPilot - Study Service API")
                        .version("v1")
                        .description("개념/미니체크/객관식/실기/보조학습/리포트 API"))
                .externalDocs(new ExternalDocumentation()
                        .description("Gateway: /api/study/**"))
                // JWT 보안 스키마 등록
                .components(new Components()
                        .addSecuritySchemes(SECURITY_SCHEME_NAME,
                                new SecurityScheme()
                                        .name(SECURITY_SCHEME_NAME)
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                        )
                )
                // 모든 엔드포인트에 JWT 필요하도록 설정
                .addSecurityItem(new SecurityRequirement().addList(SECURITY_SCHEME_NAME));
    }
}
