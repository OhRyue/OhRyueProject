package com.OhRyue.certpilot.cert.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "cert.qnet")
public class QnetProperties {

  /**
   * Raw service key. It will be URL-encoded per request.
   */
  private String key;

  /**
   * Base URL for primary Q-Net APIs.
   */
  private String baseUrl;

  /**
   * Base URL for data.go.kr Q-Net APIs.
   */
  private String dataUrl;

  /**
   * Timeout in milliseconds.
   */
  private int timeout = 3000;

  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
  }

  public String getBaseUrl() {
    return baseUrl;
  }

  public void setBaseUrl(String baseUrl) {
    this.baseUrl = baseUrl;
  }

  public String getDataUrl() {
    return dataUrl;
  }

  public void setDataUrl(String dataUrl) {
    this.dataUrl = dataUrl;
  }

  public int getTimeout() {
    return timeout;
  }

  public void setTimeout(int timeout) {
    this.timeout = timeout;
  }
}

