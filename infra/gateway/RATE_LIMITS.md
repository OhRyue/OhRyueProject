# Gateway Rate Limiting

The API Gateway enforces a Redis-backed rate limiter for every routed request.

| Scope | Limit | Implementation |
| --- | --- | --- |
| General API traffic | **600 requests / hour** per authenticated principal (or Authorization header / client IP fallback) | `RequestRateLimiter` applied via `default-filters` in `application.yml`<br/>`replenishRate=1`, `burstCapacity=600`, `requestedTokens=6` |
| LLM-heavy endpoints | **≤ 60 requests / hour** per principal | Apply an additional `RequestRateLimiter` filter with `requestedTokens=60` to the specific route matching the LLM path (e.g. `/api/study/llm/**`). |

### Key resolver
Rate limits are calculated with the `userKeyResolver` bean:

1. Authenticated principal (`Principal#getName`)
2. Authorization header hash
3. Client IP (fallback)

### How to tighten LLM quotas
Add a dedicated route filter in `application.yml`:

```yaml
- id: study-llm
  uri: lb://study-service
  predicates:
    - Path=/api/study/llm/**
  filters:
    - name: RequestRateLimiter
      args:
        redis-rate-limiter.replenishRate: 1
        redis-rate-limiter.burstCapacity: 60
        redis-rate-limiter.requestedTokens: 60
        key-resolver: "#{@userKeyResolver}"
    - AddRequestHeader=X-Forwarded-Prefix, /api/study
    - RewritePath=/api/study/(?<segment>llm/.*), /${segment}
```

### Observability
Redis rate limiter metrics are surfaced via Spring Boot Actuator (`/actuator/metrics/gateway.requests`). Configure Prometheus scraping through the existing `management.endpoints` settings.

