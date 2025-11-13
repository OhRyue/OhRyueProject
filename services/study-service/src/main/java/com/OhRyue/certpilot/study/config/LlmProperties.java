package com.OhRyue.certpilot.study.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * LLM 서버 연동용 설정
 * - base-url, api-key, timeout-ms
 */
@Getter @Setter
@Configuration
@ConfigurationProperties(prefix = "llm")
public class LlmProperties {
  private String baseUrl = "http://llm-server:8080";
  private String apiKey = "";
  private int timeoutMs = 3000;
  private String model = "gpt-4o-mini";
}
