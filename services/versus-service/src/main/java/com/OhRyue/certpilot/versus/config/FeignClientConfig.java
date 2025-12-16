package com.OhRyue.certpilot.versus.config;

import com.OhRyue.certpilot.versus.client.FeignClientErrorDecoder;
import com.OhRyue.certpilot.versus.security.InternalTokenProvider;
import feign.Logger;
import feign.RequestInterceptor;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Slf4j
@Configuration
@RequiredArgsConstructor
public class FeignClientConfig {

  private final InternalTokenProvider internalTokenProvider;

  @Bean
  public FeignClientErrorDecoder feignClientErrorDecoder(MonitoringConfig monitoringConfig) {
    return new FeignClientErrorDecoder(monitoringConfig);
  }

  @Bean
  public Logger.Level feignLoggerLevel() {
    // 디버깅할 때 FULL로 올리면 헤더/바디까지 로그가 과해질 수 있으니 BASIC 유지
    return Logger.Level.BASIC;
  }

  /**
   * Feign 요청 인터셉터
   *
   * 내부 호출 규칙(확정):
   * - targetName == "study-service"    -> Internal JWT (aud=study-service)
   * - targetName == "progress-service" -> Internal JWT (aud=progress-service)
   *
   * 그 외:
   * - 사용자 JWT forwarding
   */
  @Bean
  public RequestInterceptor requestInterceptor() {
    return template -> {
      try {
        String path = safe(template.url()); // 보통 "/api/xxx/yyy"
        String method = safe(template.method());

        String targetName = (template.feignTarget() != null) ? safe(template.feignTarget().name()) : "unknown";
        String targetUrl  = (template.feignTarget() != null) ? safe(template.feignTarget().url())  : "";

        // 1) 가장 안정적인 내부 호출 판정: FeignClient name 기반
        boolean isStudyTarget = "study-service".equalsIgnoreCase(targetName);
        boolean isProgressTarget = "progress-service".equalsIgnoreCase(targetName);

        // 2) 예외 케이스 대비: targetName이 unknown인 경우에만 URL로 추론(보조)
        boolean isUnknownTarget = "unknown".equalsIgnoreCase(targetName) || targetName.isBlank();
        boolean looksLikeStudy = containsAnyIgnoreCase(path, targetUrl, "/api/study/versus");
        boolean looksLikeProgress = containsAnyIgnoreCase(path, targetUrl, "/api/progress/versus");

        boolean isInternalCall = isStudyTarget || isProgressTarget || (isUnknownTarget && (looksLikeStudy || looksLikeProgress));

        if (isInternalCall) {
          String audience = isProgressTarget ? "progress-service"
              : (isStudyTarget ? "study-service"
              : (looksLikeProgress ? "progress-service" : "study-service"));

          String internalToken = internalTokenProvider.generateInternalToken(audience);

          // 항상 덮어쓰기
          template.header("Authorization");
          template.header("Authorization", "Bearer " + internalToken);

          log.info("[versus-service] ✅ Feign internal call -> attach INTERNAL JWT. targetName={}, audience={}, method={}, path={}",
              targetName, audience, method, path);
          return;
        }

        // ===== 내부 호출이 아니면 사용자 JWT 전달 =====
        String authorization = resolveUserAuthorization();

        if (authorization != null && !authorization.isBlank()) {
          template.header("Authorization");
          template.header("Authorization", authorization);
          log.debug("[versus-service] Feign user call -> forward USER JWT. targetName={}, method={}, path={}",
              targetName, method, path);
        } else {
          log.debug("[versus-service] Feign user call -> no Authorization. targetName={}, method={}, path={}",
              targetName, method, path);
        }

      } catch (Exception e) {
        log.error("[versus-service] Feign JWT 처리 중 오류", e);
      }
    };
  }

  private static String resolveUserAuthorization() {
    String authorization = null;

    ServletRequestAttributes attributes =
        (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();

    if (attributes != null) {
      HttpServletRequest request = attributes.getRequest();
      authorization = request.getHeader("Authorization");
    }

    // 비동기 fallback (ThreadLocal)
    if ((authorization == null || authorization.isBlank()) && AsyncConfig.getJwtToken() != null) {
      authorization = AsyncConfig.getJwtToken();
      log.debug("[versus-service] 비동기 작업에서 ThreadLocal JWT 사용");
    }
    return authorization;
  }

  private static String safe(String s) {
    return (s == null) ? "" : s;
  }

  private static boolean containsAnyIgnoreCase(String a, String b, String needle) {
    String x = safe(a).toLowerCase();
    String y = safe(b).toLowerCase();
    String n = safe(needle).toLowerCase();
    return x.contains(n) || y.contains(n);
  }
}
