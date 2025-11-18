package com.OhRyue.certpilot.versus.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI versusOpenAPI() {
        String jwtSchemeName = "JWT";

        // Security 요구사항: 기본적으로 JWT 필요
        SecurityRequirement securityRequirement = new SecurityRequirement().addList(jwtSchemeName);

        // Security 스키마 정의 (Bearer 토큰)
        SecurityScheme securityScheme = new SecurityScheme()
                .name(jwtSchemeName)
                .type(SecurityScheme.Type.HTTP)
                .scheme("Bearer")
                .bearerFormat("JWT");

        return new OpenAPI()
                .info(new Info()
                        .title("CertPilot - Versus Service API")
                        .description("Versus-Service API (1:1 배틀 / 토너먼트 / 골든벨) with JWT Auth")
                        .version("v1.0"))
                .components(new Components().addSecuritySchemes(jwtSchemeName, securityScheme))
                .addSecurityItem(securityRequirement);
    }
}
