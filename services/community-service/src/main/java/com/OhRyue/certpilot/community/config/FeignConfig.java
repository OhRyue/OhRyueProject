package com.OhRyue.certpilot.community.config;

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
 * - JWT 토큰 전달 (RequestInterceptor)
 * - 로깅 레벨 설정
 */
@Slf4j
@Configuration
public class FeignConfig {

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
                    ServletRequestAttributes attributes = 
                        (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
                    
                    if (attributes != null) {
                        HttpServletRequest request = attributes.getRequest();
                        String authorization = request.getHeader("Authorization");
                        
                        if (authorization != null && !authorization.isBlank()) {
                            template.header("Authorization", authorization);
                            log.debug("Feign Client에 JWT 토큰 전달: url={}, method={}", 
                                    template.url(), template.method());
                        } else {
                            log.warn("Feign Client 요청에 Authorization 헤더가 없습니다. url={}, method={}", 
                                    template.url(), template.method());
                        }
                    } else {
                        log.warn("RequestContextHolder에서 RequestAttributes를 가져올 수 없습니다. url={}, method={}", 
                                template.url(), template.method());
                    }
                } catch (Exception e) {
                    log.error("Feign Client에 JWT 토큰 전달 중 오류 발생: url={}, method={}, error={}", 
                            template.url(), template.method(), e.getMessage(), e);
                }
            }
        };
    }
}









