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
import java.util.List;

@Component
public class InternalJwtAuthFilter extends OncePerRequestFilter {

  private static final Logger log = LoggerFactory.getLogger(InternalJwtAuthFilter.class);
  private static final String INTERNAL_PATH = "/api/study/versus/";
  private static final String ROLE_INTERNAL = "ROLE_INTERNAL";

  private final JwtUtil internalJwtUtil;

  public InternalJwtAuthFilter(@Value("${auth.internal.jwt.secret}") String secret) {
    this.internalJwtUtil = new JwtUtil(secret);
  }

  @Override
  protected boolean shouldNotFilter(HttpServletRequest request) {
    return !request.getRequestURI().startsWith(INTERNAL_PATH);
  }

  @Override
  protected void doFilterInternal(HttpServletRequest request,
                                  HttpServletResponse response,
                                  FilterChain chain) throws ServletException, IOException {

    if (SecurityContextHolder.getContext().getAuthentication() != null) {
      chain.doFilter(request, response);
      return;
    }

    String auth = request.getHeader("Authorization");
    if (!StringUtils.hasText(auth) || !auth.startsWith("Bearer ")) {
      response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT required");
      return;
    }

    String token = auth.substring(7);

    try {
      if (internalJwtUtil.isExpired(token)) {
        response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT expired");
        return;
      }

      var authToken = new UsernamePasswordAuthenticationToken(
          internalJwtUtil.getUserId(token),
          null,
          List.of(new SimpleGrantedAuthority(ROLE_INTERNAL))
      );
      authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
      SecurityContextHolder.getContext().setAuthentication(authToken);

      log.info("✅ Internal JWT 인증 성공: {}", request.getRequestURI());
      chain.doFilter(request, response);

    } catch (Exception e) {
      SecurityContextHolder.clearContext();
      response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT invalid");
    }
  }
}
