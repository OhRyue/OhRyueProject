package com.OhRyue.certpilot.cert.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "cert.qnet")
public class QnetProperties {

    /**
     * - 코드에서는 별도의 URLEncoder 처리 없이 그대로 serviceKey 파라미터로 사용합니다.
     *   (Feign 이 쿼리스트링을 알아서 인코딩해 주기 때문에, 여기서 다시 인코딩하면 이중 인코딩 문제가 발생합니다.)
     */
    private String key;

    /**
     * Q-Net OpenAPI 기본 URL
     * 예) https://openapi.q-net.or.kr/api/service/rest
     */
    private String baseUrl;

    /**
     * data.go.kr (B490007) Q-Net 연계 OpenAPI 기본 URL
     * 예) https://apis.data.go.kr/B490007
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
