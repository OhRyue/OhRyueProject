package com.OhRyue.certpilot.versus.config;

import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskDecorator;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * 비동기 작업 설정
 * - SecurityContext와 RequestContext를 비동기 작업에 전파
 * - Feign Client에서 JWT 토큰을 사용할 수 있도록 함
 */
@Slf4j
@Configuration
@EnableAsync
public class AsyncConfig {

    /**
     * ThreadLocal에 JWT 토큰 저장 (비동기 스레드에서 Feign Client가 사용)
     */
    private static final ThreadLocal<String> JWT_TOKEN_HOLDER = new ThreadLocal<>();

    /**
     * 현재 스레드의 JWT 토큰 가져오기
     */
    public static String getJwtToken() {
        return JWT_TOKEN_HOLDER.get();
    }

    /**
     * JWT 토큰 설정 (비동기 작업 시작 전에 호출)
     */
    public static void setJwtToken(String jwtToken) {
        if (jwtToken != null && !jwtToken.isBlank()) {
            JWT_TOKEN_HOLDER.set(jwtToken);
        }
    }

    /**
     * JWT 토큰 정리 (비동기 작업 완료 후 호출)
     */
    public static void clearJwtToken() {
        JWT_TOKEN_HOLDER.remove();
    }

    @Bean(name = "taskExecutor")
    public Executor taskExecutor() {
        return Executors.newFixedThreadPool(10, r -> {
            Thread t = new Thread(r);
            t.setDaemon(true);
            t.setName("versus-async-" + t.getId());
            return t;
        });
    }

    @Bean
    public TaskDecorator taskDecorator() {
        return new ContextCopyingTaskDecorator();
    }

    /**
     * SecurityContext와 RequestContext를 비동기 작업에 전파하는 TaskDecorator
     */
    private static class ContextCopyingTaskDecorator implements TaskDecorator {
        @Override
        public Runnable decorate(Runnable runnable) {
            // 현재 스레드의 컨텍스트 저장
            SecurityContext securityContext = SecurityContextHolder.getContext();
            RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
            
            // 원래 요청에서 Authorization 헤더 추출
            String jwtToken = null;
            if (requestAttributes instanceof ServletRequestAttributes) {
                HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
                String authHeader = request.getHeader("Authorization");
                if (authHeader != null && !authHeader.isBlank()) {
                    jwtToken = authHeader;
                    log.debug("비동기 작업에 JWT 토큰 전달: {}", authHeader.substring(0, Math.min(20, authHeader.length())) + "...");
                }
            }

            final String token = jwtToken;

            return () -> {
                try {
                    // 비동기 스레드에 컨텍스트 복사
                    if (securityContext != null) {
                        SecurityContextHolder.setContext(securityContext);
                    }
                    if (requestAttributes != null) {
                        RequestContextHolder.setRequestAttributes(requestAttributes, true);
                    }
                    
                    // JWT 토큰을 ThreadLocal에 저장
                    if (token != null) {
                        JWT_TOKEN_HOLDER.set(token);
                    }
                    
                    runnable.run();
                } finally {
                    // 작업 완료 후 컨텍스트 정리
                    SecurityContextHolder.clearContext();
                    RequestContextHolder.resetRequestAttributes();
                    JWT_TOKEN_HOLDER.remove();
                }
            };
        }
    }
}

