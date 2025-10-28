package com.OhRyue.certpilot.gateway.filter;

import io.micrometer.tracing.Span;
import io.micrometer.tracing.Tracer;
import org.springframework.cloud.gateway.filter.GlobalFilter;
import org.springframework.cloud.gateway.filter.GatewayFilterChain;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

@Component
public class TraceHeaderFilter implements GlobalFilter, Ordered {
  private final Tracer tracer;

  public TraceHeaderFilter(Tracer tracer) {
    this.tracer = tracer;
  }

  @Override
  public Mono<Void> filter(ServerWebExchange exchange, GatewayFilterChain chain) {
    return chain.filter(exchange).then(Mono.fromRunnable(() -> {
      Span current = tracer.currentSpan();
      if (current != null) {
        exchange.getResponse().getHeaders().add("X-Trace-Id", current.context().traceId());
      }
    }));
  }

  @Override
  public int getOrder() { return -1; }
}
