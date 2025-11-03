package com.OhRyue.certpilot.study.service.llm;

import com.OhRyue.certpilot.study.config.LlmProperties; // ← 공백 제거된 정확한 경로
import feign.Logger;
import feign.Request;
import feign.RequestInterceptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class LlmFeignConfig {

    @Bean
    Logger.Level feignLoggerLevel() {
        return Logger.Level.BASIC;
    }

    @Bean
    Request.Options feignOptions(LlmProperties props) {
        int ms = Math.max(1000, props.getTimeoutMs());
        return new Request.Options(ms, ms);
    }

    @Bean
    RequestInterceptor authHeader(LlmProperties props) {
        return template -> {
            if (props.getApiKey() != null && !props.getApiKey().isBlank()) {
                template.header("Authorization", "Bearer " + props.getApiKey());
            }
        };
    }
}
