package com.OhRyue.certpilot.gateway.config;

import org.springframework.cloud.gateway.filter.ratelimit.KeyResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.security.Principal;
import java.util.Objects;
import java.util.Optional;

@Configuration
public class RateLimitConfig {

  /** RequestRateLimiter에서 사용할 사용자 키 (단일 정의) */
  @Bean(name = "userKeyResolver")
  public KeyResolver userKeyResolver() {
    return (ServerWebExchange exchange) -> {
      // 1) JWT Principal(sub 등) 우선
      Mono<String> byPrincipal = exchange.getPrincipal()
          .map(Principal::getName)
          .filter(Objects::nonNull)
          .map(name -> "usr:" + name);

      // 2) Authorization 헤더 해시
      Mono<String> byAuthHeader = Mono.fromSupplier(() ->
              Optional.ofNullable(exchange.getRequest().getHeaders().getFirst("Authorization"))
                  .filter(s -> !s.isBlank())
                  .map(s -> "aut:" + Integer.toHexString(s.hashCode()))
                  .orElse(null)
          )
          .filter(Objects::nonNull);

      // 3) 최종 폴백: IP
      Mono<String> byIp = Mono.fromSupplier(() -> "ip:" + clientIp(exchange));

      return byPrincipal.switchIfEmpty(byAuthHeader).switchIfEmpty(byIp);
    };
  }

  private static String clientIp(ServerWebExchange exchange) {
    String xf = Optional.ofNullable(exchange.getRequest().getHeaders().getFirst("X-Forwarded-For")).orElse("");
    if (!xf.isBlank()) return xf.split(",")[0].trim();
    var addr = exchange.getRequest().getRemoteAddress();
    return (addr == null) ? "unknown" : addr.getAddress().getHostAddress();
  }
}
