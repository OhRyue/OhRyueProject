package com.OhRyue.certpilot.study.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "llm")
public class LlmProperties {
  /**
   * LLM 사용 여부 스위치. true면 LLM 호출, false면 폴백 문자열 사용
   */
  private boolean enabled = false;

  /**
   * LLM 게이트웨이/서버 베이스 URL (e.g., http://localhost:8099)
   */
  private String baseUrl = "http://localhost:8099";

  /**
   * LLM HTTP 타임아웃(ms)
   */
  private int timeoutMs = 5000;

  /**
   * 인증 토큰 (선택)
   */
  private String apiKey;

  // getters & setters
  public boolean isEnabled() { return enabled; }
  public void setEnabled(boolean enabled) { this.enabled = enabled; }

  public String getBaseUrl() { return baseUrl; }
  public void setBaseUrl(String baseUrl) { this.baseUrl = baseUrl; }

  public int getTimeoutMs() { return timeoutMs; }
  public void setTimeoutMs(int timeoutMs) { this.timeoutMs = timeoutMs; }

  public String getApiKey() { return apiKey; }
  public void setApiKey(String apiKey) { this.apiKey = apiKey; }
}
