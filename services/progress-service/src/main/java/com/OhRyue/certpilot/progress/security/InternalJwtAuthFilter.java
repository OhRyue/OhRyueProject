package com.OhRyue.certpilot.progress.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Component
public class InternalJwtAuthFilter extends OncePerRequestFilter {

  private final Key signingKey;
  private final String issuer;
  private final String audience;

  public InternalJwtAuthFilter(
      // yml(auth.internal.jwt.*)을 1순위로, env는 2순위로
      @Value("${auth.internal.jwt.secret:${AUTH_INTERNAL_JWT_SECRET:${INTERNAL_JWT_SECRET:}}}") String secret,
      @Value("${auth.internal.jwt.issuer:${AUTH_INTERNAL_JWT_ISSUER:${INTERNAL_JWT_ISSUER:certpilot-internal}}}") String issuer,
      @Value("${auth.internal.jwt.audience:${AUTH_INTERNAL_JWT_AUDIENCE:${INTERNAL_JWT_AUDIENCE:progress-service}}}") String audience
  ) {
    if (!StringUtils.hasText(secret)) {
      throw new IllegalStateException("auth.internal.jwt.secret (or AUTH_INTERNAL_JWT_SECRET/INTERNAL_JWT_SECRET) is required");
    }
    this.signingKey = buildKey(secret);
    this.issuer = issuer;
    this.audience = audience;

    log.info("[progress-service] InternalJwtAuthFilter initialized: issuer={}, audience={}", this.issuer, this.audience);
  }

  @Override
  protected boolean shouldNotFilter(HttpServletRequest request) {
    // 내부 전용: versus 결과/보상 지급
    String path = request.getRequestURI();
    return path == null || !path.startsWith("/api/progress/versus/");
  }

  @Override
  protected void doFilterInternal(
      HttpServletRequest request,
      HttpServletResponse response,
      FilterChain filterChain
  ) throws ServletException, IOException {

    String path = request.getRequestURI();
    String authHeader = request.getHeader("Authorization");

    // Authorization 없으면 여기서 401을 만들 필요는 없음.
    // SecurityConfig의 hasRole(INTERNAL)에서 최종 차단됩니다.
    if (!StringUtils.hasText(authHeader) || !authHeader.startsWith("Bearer ")) {
      filterChain.doFilter(request, response);
      return;
    }

    String token = authHeader.substring(7);

    try {
      Jws<Claims> jws = Jwts.parserBuilder()
          .requireIssuer(issuer)       // issuer 검증
          .requireAudience(audience)   // audience 검증 (progress-service 여야 함)
          .setSigningKey(signingKey)   // 서명 검증
          .build()
          .parseClaimsJws(token);

      Claims claims = jws.getBody();
      String subject = claims.getSubject(); // internal-versus-service

      List<String> roles = extractRoles(claims.get("roles"));
      boolean hasInternalRole = roles.stream().anyMatch(r ->
          "INTERNAL".equals(r) || "ROLE_INTERNAL".equals(r)
      );

      if (!hasInternalRole) {
        throw new IllegalArgumentException("INTERNAL role missing");
      }

      if (SecurityContextHolder.getContext().getAuthentication() == null) {
        var auth = new UsernamePasswordAuthenticationToken(
            subject,
            null,
            List.of(new SimpleGrantedAuthority("ROLE_INTERNAL"))
        );
        SecurityContextHolder.getContext().setAuthentication(auth);
      }

      log.debug("[progress-service] ✅ Internal JWT 인증 성공 - sub: {}, iss: {}, aud: {}, path: {}",
          subject, issuer, audience, path);

    } catch (Exception e) {
      log.warn("[progress-service] ❌ Internal JWT 인증 실패 - path: {}, error: {}", path, e.getMessage());
      SecurityContextHolder.clearContext();
    }

    filterChain.doFilter(request, response);
  }

  private Key buildKey(String secret) {
    // base64 우선 → 실패 시 raw bytes
    try {
      byte[] keyBytes = Decoders.BASE64.decode(secret);
      return Keys.hmacShaKeyFor(keyBytes);
    } catch (Exception ignore) {
      return Keys.hmacShaKeyFor(secret.getBytes(StandardCharsets.UTF_8));
    }
  }

  private List<String> extractRoles(Object rolesObj) {
    if (rolesObj == null) return List.of();

    // roles: ["INTERNAL"]
    if (rolesObj instanceof List<?> list) {
      List<String> out = new ArrayList<>();
      for (Object o : list) out.add(String.valueOf(o));
      return out;
    }

    // roles: "INTERNAL"
    if (rolesObj instanceof String s) {
      return List.of(s);
    }

    return List.of(String.valueOf(rolesObj));
  }
}
