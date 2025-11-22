package com.OhRyue.certpilot.gateway.filter;

import org.springframework.cloud.gateway.filter.GatewayFilterChain;
import org.springframework.cloud.gateway.filter.GlobalFilter;
import org.springframework.core.Ordered;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.List;

@Component
public class CorsGlobalFilter implements GlobalFilter, Ordered {

    private static final List<String> ALLOWED_ORIGINS = Arrays.asList(
            "http://localhost:3000",
            "https://mycertpilot.com",
            "https://www.mycertpilot.com"
    );

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, GatewayFilterChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        ServerHttpResponse response = exchange.getResponse();
        HttpHeaders headers = response.getHeaders();

        String origin = request.getHeaders().getFirst(HttpHeaders.ORIGIN);
        
        // OPTIONS 요청 (preflight) 처리 - 항상 CORS 헤더 필요
        if (request.getMethod() == HttpMethod.OPTIONS) {
            if (origin != null && isAllowedOrigin(origin)) {
                headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, origin);
                headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
                headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_METHODS, "GET, POST, PUT, DELETE, OPTIONS, PATCH");
                headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_HEADERS, "*");
                headers.add(HttpHeaders.ACCESS_CONTROL_MAX_AGE, "3600");
            }
            response.setStatusCode(HttpStatus.OK);
            return response.setComplete();
        }

        // 일반 요청에 대해 CORS 헤더 추가 (모든 응답에 필요)
        if (origin != null && isAllowedOrigin(origin)) {
            headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, origin);
            headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
            headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_METHODS, "GET, POST, PUT, DELETE, OPTIONS, PATCH");
            headers.add(HttpHeaders.ACCESS_CONTROL_ALLOW_HEADERS, "*");
        }

        return chain.filter(exchange);
    }

    private boolean isAllowedOrigin(String origin) {
        return ALLOWED_ORIGINS.contains(origin);
    }

    @Override
    public int getOrder() {
        // CORS 필터는 가장 먼저 실행되어야 함
        return -100;
    }
}

