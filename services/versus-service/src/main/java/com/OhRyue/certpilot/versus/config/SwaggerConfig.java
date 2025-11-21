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
        String schemeName = "bearerAuth";

        SecurityRequirement securityRequirement = new SecurityRequirement().addList(schemeName);

        SecurityScheme securityScheme = new SecurityScheme()
                .name(schemeName)
                .type(SecurityScheme.Type.HTTP)
                .scheme("bearer")
                .bearerFormat("JWT");

        return new OpenAPI()
                .info(new Info()
                        .title("CertPilot - Versus Service API")
                        .description("Versus-Service API (1:1 배틀 / 토너먼트 / 골든벨) with JWT Auth")
                        .version("v1.0"))
                .components(new Components().addSecuritySchemes(schemeName, securityScheme))
                .addSecurityItem(securityRequirement);
    }
}
