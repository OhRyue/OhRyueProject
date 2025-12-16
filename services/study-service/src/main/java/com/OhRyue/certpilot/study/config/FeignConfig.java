package com.OhRyue.certpilot.study.config;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Component
public class FeignConfig implements RequestInterceptor {

  @Override
  public void apply(RequestTemplate template) {
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
  }
}
