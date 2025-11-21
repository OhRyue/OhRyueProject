package com.OhRyue.certpilot.gateway.security;

import com.OhRyue.common.security.JwtUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.gateway.filter.GlobalFilter;
import org.springframework.core.Ordered;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

@Component
public class JwtAuthFilter implements GlobalFilter, Ordered {

    private static final Logger log = LoggerFactory.getLogger(JwtAuthFilter.class);

    private final JwtUtil jwtUtil;

    public JwtAuthFilter(@Value("${auth.jwt.secret}") String secret) {
        log.debug("🔑 [GATEWAY] JwtAuthFilter 초기화 - auth.jwt.secret 길이: {} chars", secret != null ? secret.length() : 0);
        this.jwtUtil = new JwtUtil(secret);
        log.debug("✅ [GATEWAY] JwtAuthFilter 초기화 완료");
    }
    @Override
    public Mono<Void> filter(ServerWebExchange exchange,
                             org.springframework.cloud.gateway.filter.GatewayFilterChain chain) {

        String path = exchange.getRequest().getPath().value();

        // 1) 공개 경로(로그인, 회원가입, 비밀번호 찾기, health, swagger)는 패스
        if (isPublicPath(path)) {
            return chain.filter(exchange);
        }

        String authHeader = exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return unauthorized(exchange, "Missing or invalid Authorization header");
        }

        try {
            String userId = jwtUtil.getUserId(authHeader);
            if (userId == null || userId.isBlank()) {
                return unauthorized(exchange, "Invalid token: no subject");
            }

            if (jwtUtil.isExpired(authHeader)) {
                return unauthorized(exchange, "Token expired");
            }

            // 2) downstream 서비스로 userId/roles를 헤더로 전달
            String[] rolesArr = jwtUtil.getRoles(authHeader);
            String roles = String.join(",", rolesArr);

            var mutatedRequest = exchange.getRequest().mutate()
                    .header("X-User-Id", userId)
                    .header("X-User-Roles", roles)
                    // Authorization 헤더는 그대로 유지
                    .build();

            var mutatedExchange = exchange.mutate().request(mutatedRequest).build();
            return chain.filter(mutatedExchange);

        } catch (Exception e) {
            log.warn("JWT validation failed: {}", e.getMessage());
            return unauthorized(exchange, "Token validation failed");
        }
    }

    private boolean isPublicPath(String path) {

        // 1) Account - 로그인 이전 공개 API
        return path.startsWith("/api/account/login")              // 로그인
                || path.startsWith("/api/account/register")           // 회원가입
                || path.startsWith("/api/account/check-userId")       // 아이디 중복 확인
                || path.startsWith("/api/account/send-verification")  // 인증 이메일 전송
                || path.startsWith("/api/account/verify-email")       // 이메일 인증 확인
                || path.startsWith("/api/account/reset-password")     // 비밀번호 재설정
                || path.startsWith("/api/account/refresh")            // 토큰 재발급

                // 2) 공통 공개 API (health, swagger)
                || path.startsWith("/actuator")
                || path.startsWith("/v3/api-docs")
                || path.startsWith("/swagger-ui")
                || path.startsWith("/swagger-ui.html");
    }


    private Mono<Void> unauthorized(ServerWebExchange exchange, String msg) {
        exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
        exchange.getResponse().getHeaders().add("X-Auth-Error", msg);
        return exchange.getResponse().setComplete();
    }

    @Override
    public int getOrder() {
        // CORS 필터 뒤, RateLimiter와의 위치는 필요시 조정
        return -1;
    }
}
