package com.OhRyue.certpilot.study.config;

import com.OhRyue.certpilot.study.security.InternalTokenProvider;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Slf4j
@Component
@RequiredArgsConstructor
public class FeignConfig implements RequestInterceptor {

  private final InternalTokenProvider internalTokenProvider;

  @Override
  public void apply(RequestTemplate template) {
    try {
      String path = safe(template.url()); // 보통 "/api/cert/topics" 등
      String method = safe(template.method());
      
      String targetName = (template.feignTarget() != null) ? safe(template.feignTarget().name()) : "unknown";
      String targetUrl = (template.feignTarget() != null) ? safe(template.feignTarget().url()) : "";

      // cert-service 호출 판정: FeignClient name 기반
      boolean isCertTarget = "cert-service".equalsIgnoreCase(targetName);
      boolean looksLikeCert = containsAnyIgnoreCase(path, targetUrl, "/api/cert");

      boolean isInternalCall = isCertTarget || (targetName.equals("unknown") && looksLikeCert);

      if (isInternalCall) {
        // cert-service 호출 시 Internal JWT 자동 첨부
        String audience = "cert-service";
        String internalToken = internalTokenProvider.generateInternalToken(audience);

        // 기존 Authorization 헤더 제거 후 Internal JWT 추가
        template.removeHeader("Authorization");
        template.header("Authorization", "Bearer " + internalToken);

        log.info("[study] ✅ Feign internal call -> attach INTERNAL JWT. targetName={}, audience={}, method={}, path={}",
            targetName, audience, method, path);
        return;
      }

      // ===== 내부 호출이 아니면 사용자 JWT 전달 =====
      ServletRequestAttributes attributes =
          (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();

      if (attributes == null) return;

      HttpServletRequest request = attributes.getRequest();

      String url = template.url(); // 상대경로
      boolean isInternalPath = StringUtils.hasText(url) && url.contains("/internal");

      // 내부 API 호출에는 유저 Authorization 전달 금지 (401 예방)
      if (!isInternalPath) {
        String authHeader = request.getHeader("Authorization");
        if (StringUtils.hasText(authHeader)) {
          template.header("Authorization", authHeader);
        }
      }

      // Gateway에서 설정한 헤더 전달
      String userId = request.getHeader("X-User-Id");
      if (StringUtils.hasText(userId)) {
        template.header("X-User-Id", userId);
      }

      String roles = request.getHeader("X-User-Roles");
      if (StringUtils.hasText(roles)) {
        template.header("X-User-Roles", roles);
      }
    } catch (Exception e) {
      log.error("[study] Feign JWT 처리 중 오류", e);
    }
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
