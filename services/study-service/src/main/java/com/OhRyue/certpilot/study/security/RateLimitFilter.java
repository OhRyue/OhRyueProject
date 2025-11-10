package com.OhRyue.certpilot.study.security;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 매우 단순한 in-memory 레이트리밋(운영 전 Redis/게이트웨이로 대체 권장)
 * - key: Authorization header or remoteAddr
 * - 윈도우: 1초, 용량: 10 req
 */
public class RateLimitFilter implements Filter {

  private static final int CAPACITY = 10;
  private static final long WINDOW_MS = 1000;

  private static class Bucket {
    int tokens;
    long windowStart;
  }
  private final Map<String, Bucket> buckets = new ConcurrentHashMap<>();

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
    var req = (HttpServletRequest) request;
    var resp = (HttpServletResponse) response;

    String key = rateKey(req);
    Bucket b = buckets.computeIfAbsent(key, k -> {
      Bucket nb = new Bucket();
      nb.tokens = CAPACITY;
      nb.windowStart = Instant.now().toEpochMilli();
      return nb;
    });

    synchronized (b) {
      long now = Instant.now().toEpochMilli();
      if (now - b.windowStart >= WINDOW_MS) {
        b.tokens = CAPACITY;
        b.windowStart = now;
      }
      if (b.tokens <= 0) {
        resp.setStatus(429);
        resp.setContentType("application/json");
        resp.getWriter().write("{\"message\":\"Too Many Requests\"}");
        return;
      }
      b.tokens--;
    }
    chain.doFilter(request, response);
  }

  private String rateKey(HttpServletRequest req) {
    String auth = req.getHeader("Authorization");
    if (StringUtils.hasText(auth)) return auth;
    return req.getRemoteAddr();
  }
}
