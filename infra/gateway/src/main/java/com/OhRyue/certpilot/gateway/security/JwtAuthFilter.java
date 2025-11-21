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
        this.jwtUtil = new JwtUtil(secret);
    }

    @Override
    public Mono<Void> filter(ServerWebExchange exchange,
                             org.springframework.cloud.gateway.filter.GatewayFilterChain chain) {

        String path = exchange.getRequest().getPath().value();
        log.debug("[JwtAuthFilter] path = {}", path);

        // 1) 공개 경로(로그인/회원가입/비번찾기/health/swagger)는 JWT 검사 없이 통과
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

            String[] rolesArr = jwtUtil.getRoles(authHeader);
            String roles = String.join(",", rolesArr);

            var mutatedRequest = exchange.getRequest().mutate()
                    .header("X-User-Id", userId)
                    .header("X-User-Roles", roles)
                    .build();

            var mutatedExchange = exchange.mutate().request(mutatedRequest).build();
            return chain.filter(mutatedExchange);

        } catch (Exception e) {
            log.warn("JWT validation failed: {}", e.getMessage());
            return unauthorized(exchange, "Token validation failed");
        }
    }

    /**
     * 게이트웨이 레벨에서 JWT 없이 허용할 공개 경로들
     */
    private boolean isPublicPath(String path) {
        if (path == null || path.isBlank()) return false;

        // account 관련 공개 API는 전부 JWT 없이 통과
        //    -> /api/account/login, /api/account/send-verification 등 모두 포함
        if (path.startsWith("/api/account")) {
            return true;
        }

        // health / swagger
        if (path.startsWith("/actuator")) return true;
        if (path.startsWith("/v3/api-docs")) return true;
        if (path.startsWith("/swagger-ui")) return true;
        if (path.startsWith("/swagger-ui.html")) return true;

        return false;
    }

    private Mono<Void> unauthorized(ServerWebExchange exchange, String msg) {
        exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
        exchange.getResponse().getHeaders().add("X-Auth-Error", msg);
        return exchange.getResponse().setComplete();
    }

    @Override
    public int getOrder() {
        return -1;
    }
}
