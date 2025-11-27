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
     */
    @Bean
    public RequestInterceptor requestInterceptor() {
        return new RequestInterceptor() {
            @Override
            public void apply(RequestTemplate template) {
                try {
                    // 현재 HTTP 요청에서 Authorization 헤더 가져오기
                    ServletRequestAttributes attributes = 
                        (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
                    
                    if (attributes != null) {
                        HttpServletRequest request = attributes.getRequest();
                        String authorization = request.getHeader("Authorization");
                        
                        if (authorization != null && !authorization.isBlank()) {
                            template.header("Authorization", authorization);
                            log.debug("Feign Client에 JWT 토큰 전달: {}", authorization.substring(0, Math.min(20, authorization.length())) + "...");
                        } else {
                            log.warn("Feign Client 요청에 Authorization 헤더가 없습니다.");
                        }
                    } else {
                        log.warn("RequestContextHolder에서 요청 정보를 가져올 수 없습니다.");
                    }
                } catch (Exception e) {
                    log.error("Feign Client에 JWT 토큰 전달 중 오류 발생: {}", e.getMessage(), e);
                }
            }
        };
    }
}
