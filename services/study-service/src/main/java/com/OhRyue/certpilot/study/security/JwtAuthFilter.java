package com.OhRyue.certpilot.study.security;

import com.OhRyue.common.security.JwtUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

@Component
public class JwtAuthFilter extends OncePerRequestFilter {

  private static final Logger log = LoggerFactory.getLogger(JwtAuthFilter.class);

  /**
   * Internal API 경로는 InternalJwtAuthFilter 전용
   * - 여기서 절대 건드리면 안 됩니다.
   * - (이 필터가 SecurityContext를 덮어쓰면 hasRole(INTERNAL)에서 403 발생 가능)
   */
  private static final String INTERNAL_PATH_PREFIX = "/api/study/versus/";

  private final JwtUtil jwtUtil;

  public JwtAuthFilter(@Value("${auth.jwt.secret}") String secret) {
    this.jwtUtil = new JwtUtil(secret);
  }

  /**
   * Internal 전용 경로(/api/study/versus/**)는 이 필터 자체를 실행하지 않음.
   * -> InternalJwtAuthFilter만 실행되도록 보장
   */
  @Override
  protected boolean shouldNotFilter(HttpServletRequest request) {
    String path = request.getRequestURI();
    return path != null && path.startsWith(INTERNAL_PATH_PREFIX);
  }

  @Override
  protected void doFilterInternal(HttpServletRequest request,
                                  HttpServletResponse response,
                                  FilterChain filterChain) throws ServletException, IOException {

    String path = request.getRequestURI();

    // Public 경로는 인증 없이 통과
    if (isPublicPath(path)) {
      filterChain.doFilter(request, response);
      return;
    }

    String userId = null;
    String[] roles = new String[0];

    try {
      // 1) Gateway 헤더 우선 사용
      String userIdHeader = request.getHeader("X-User-Id");
      String rolesHeader = request.getHeader("X-User-Roles");

      if (StringUtils.hasText(userIdHeader)) {
        userId = userIdHeader;
        if (StringUtils.hasText(rolesHeader)) {
          roles = Arrays.stream(rolesHeader.split(","))
              .map(String::trim)
              .filter(StringUtils::hasText)
              .toArray(String[]::new);
        }
      }
      // 2) Gateway 우회 호출 대비: Authorization JWT 직접 파싱
      else {
        String authHeader = request.getHeader("Authorization");
        if (StringUtils.hasText(authHeader) && authHeader.startsWith("Bearer ")) {
          String token = authHeader.substring(7); // Bearer 제거
          userId = jwtUtil.getUserId(token);
          roles = jwtUtil.getRoles(token);
        }
      }

      // SecurityContext 설정 (이미 있으면 건드리지 않음)
      if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {

        var authorities = Arrays.stream(roles)
            .filter(StringUtils::hasText)
            .map(r -> r.startsWith("ROLE_") ? r : "ROLE_" + r)
            .map(SimpleGrantedAuthority::new)
            .collect(Collectors.toList());

        var auth = new UsernamePasswordAuthenticationToken(
            userId,
            null,
            authorities
        );
        auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
        SecurityContextHolder.getContext().setAuthentication(auth);

        log.debug("[study-service] ✅ User JWT 인증 성공 - userId={}, roles={}, path={}",
            userId, Arrays.toString(roles), path);
      }

    } catch (Exception e) {
      // 예외 발생 시 기존 인증을 무조건 지우지 않음
      log.warn("[study-service] ❌ JWT 인증/파싱 실패 - path={}, error={}",
          path, e.getMessage());

      // 기존 인증이 없다면 clear
      if (SecurityContextHolder.getContext().getAuthentication() == null) {
        SecurityContextHolder.clearContext();
      }
    }

    filterChain.doFilter(request, response);
  }

  private boolean isPublicPath(String path) {
    if (!StringUtils.hasText(path)) return true;
    return path.startsWith("/actuator")
        || path.startsWith("/v3/api-docs")
        || path.startsWith("/swagger-ui")
        || path.startsWith("/swagger-ui.html");
  }
}
