package com.OhRyue.certpilot.common.web;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.License;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@OpenAPIDefinition(
        info = @Info(
                title = "CertPilot API",
                version = "v0.1",
                description = "자격증 정보/일정/시험장/퀴즈 등 API 문서 (local)"
        )
)
@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI baseOpenAPI() {
        return new OpenAPI()
                .info(new io.swagger.v3.oas.models.info.Info()
                        .title("CertPilot API")
                        .version("v0.1")
                        .description("개념·미니체크·문제·학습루프 API")
                        .contact(new Contact().name("팀 CertPilot").email("dev@certpilot.local"))
                        .license(new License().name("MIT")));
    }
}
