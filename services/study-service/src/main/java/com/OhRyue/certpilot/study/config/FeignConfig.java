package com.OhRyue.certpilot.study.config;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * FeignClient 요청에 현재 HTTP 요청의 인증 헤더를 전달하는 인터셉터
 * 다른 서비스(cert-service 등) 호출 시 JWT 토큰을 함께 전달
 */
@Component
public class FeignConfig implements RequestInterceptor {

    @Override
    public void apply(RequestTemplate template) {
        ServletRequestAttributes attributes = 
            (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        
        if (attributes != null) {
            HttpServletRequest request = attributes.getRequest();
            
            // Authorization 헤더 전달
            String authHeader = request.getHeader("Authorization");
            if (authHeader != null && !authHeader.isBlank()) {
                template.header("Authorization", authHeader);
            }
            
            // X-User-Id, X-User-Roles 헤더도 전달 (Gateway에서 설정한 헤더)
            String userId = request.getHeader("X-User-Id");
            if (userId != null && !userId.isBlank()) {
                template.header("X-User-Id", userId);
            }
            
            String roles = request.getHeader("X-User-Roles");
            if (roles != null && !roles.isBlank()) {
                template.header("X-User-Roles", roles);
            }
        }
    }
}

