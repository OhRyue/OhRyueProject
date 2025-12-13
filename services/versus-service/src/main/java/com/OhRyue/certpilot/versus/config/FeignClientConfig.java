package com.OhRyue.certpilot.versus.config;

import com.OhRyue.certpilot.versus.client.FeignClientErrorDecoder;
import feign.Logger;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Feign Client 설정
 * - Error Decoder 설정
 * - JWT 토큰 전달 (RequestInterceptor)
 * - 로깅 레벨 설정
 */
@Slf4j
@Configuration
public class FeignClientConfig {

    @Bean
    public FeignClientErrorDecoder feignClientErrorDecoder(MonitoringConfig monitoringConfig) {
        return new FeignClientErrorDecoder(monitoringConfig);
    }

    @Bean
    public Logger.Level feignLoggerLevel() {
        return Logger.Level.BASIC;
    }

    /**
     * Feign Client 요청에 JWT 토큰을 자동으로 추가하는 Interceptor
     * 현재 요청의 Authorization 헤더를 Feign 요청에도 전달
     * 비동기 작업에서는 ThreadLocal에서 JWT 토큰을 가져옴
     */
    @Bean
    public RequestInterceptor requestInterceptor() {
        return new RequestInterceptor() {
            @Override
            public void apply(RequestTemplate template) {
                try {
                    String authorization = null;
                    
                    // 1. 현재 HTTP 요청에서 Authorization 헤더 가져오기 (일반 요청)
                    ServletRequestAttributes attributes = 
                        (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
                    
                    if (attributes != null) {
                        HttpServletRequest request = attributes.getRequest();
                        authorization = request.getHeader("Authorization");
                    }
                    
                    // 2. 비동기 작업에서는 ThreadLocal에서 JWT 토큰 가져오기
                    if ((authorization == null || authorization.isBlank()) && AsyncConfig.getJwtToken() != null) {
                        authorization = AsyncConfig.getJwtToken();
                        log.debug("비동기 작업에서 ThreadLocal의 JWT 토큰 사용");
                    }
                    
                    if (authorization != null && !authorization.isBlank()) {
                        template.header("Authorization", authorization);
                        log.info("Feign Client에 JWT 토큰 전달: url={}, method={}, token={}...", 
                                template.url(), template.method(), authorization.substring(0, Math.min(30, authorization.length())));
                    } else {
                        // internal API 호출의 경우 Authorization 헤더가 없어도 정상 (permitAll 설정)
                        String url = template.url();
                        boolean isInternalApi = url != null && (url.contains("/internal/") || url.contains("/api/account/internal"));
                        if (isInternalApi) {
                            log.debug("Feign Client internal API 호출 (Authorization 없음): url={}, method={}", 
                                    url, template.method());
                        } else {
                            log.warn("Feign Client 요청에 Authorization 헤더가 없습니다. url={}, method={}, RequestContextHolder: {}, ThreadLocal: {}", 
                                    url, template.method(), attributes != null, AsyncConfig.getJwtToken() != null);
                        }
                    }
                } catch (Exception e) {
                    log.error("Feign Client에 JWT 토큰 전달 중 오류 발생: {}", e.getMessage(), e);
                }
            }
        };
    }
}
