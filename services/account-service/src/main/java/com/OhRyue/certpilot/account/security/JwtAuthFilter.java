package com.OhRyue.certpilot.account.security;

import com.OhRyue.certpilot.account.config.JwtTokenProvider;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class JwtAuthFilter extends OncePerRequestFilter {

    private static final Logger log = LoggerFactory.getLogger(JwtAuthFilter.class);
    private final JwtTokenProvider jwtTokenProvider;

    public JwtAuthFilter(JwtTokenProvider jwtTokenProvider) {
        this.jwtTokenProvider = jwtTokenProvider;
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getRequestURI();
        // JWT 없이 열어둘 경로들
        return path.startsWith("/api/account/login")
                || path.startsWith("/api/account/refresh")
                || path.startsWith("/api/account/send-verification")
                || path.startsWith("/api/account/verify-email")
                || path.startsWith("/api/account/check-userId")
                || path.startsWith("/api/account/forgot-password")
                || path.startsWith("/api/account/forgot-password/verify")
                || path.startsWith("/api/account/forgot-password/reset")
                || path.startsWith("/v3/api-docs")
                || path.startsWith("/swagger-ui")
                || path.startsWith("/swagger-ui.html")
                || path.startsWith("/actuator");
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        String requestPath = request.getRequestURI();

        String token = resolveToken(request);

        if (!StringUtils.hasText(token)) {
            // 토큰 없으면 그냥 다음 필터로 넘김 (401은 SecurityConfig 인가 규칙에서 결정)
            filterChain.doFilter(request, response);
            return;
        }

        // 토큰 만료 여부 먼저 확인
        if (jwtTokenProvider.isTokenExpired(token)) {
            log.warn("⏰ [account-service] JWT 토큰 만료 - path: {}", requestPath);
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.setHeader("WWW-Authenticate", 
                "Bearer error=\"invalid_token\", error_description=\"Jwt expired\", error_uri=\"https://tools.ietf.org/html/rfc6750#section-3.1\"");
            return;
        }

        if (jwtTokenProvider.validateToken(token)) {
            try {
                String userId = jwtTokenProvider.getUsernameFromToken(token);
                String role = jwtTokenProvider.getRoleFromToken(token);

                var auth = new UsernamePasswordAuthenticationToken(
                        userId,
                        null,
                        jwtTokenProvider.getAuthorities(role)
                );
                auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(auth);

                log.debug("✅ [account-service] JWT 인증 성공 - userId: {}, role: {}, path: {}",
                        userId, role, requestPath);
            } catch (Exception e) {
                log.error("❌ [account-service] JWT 파싱 중 오류 - path: {}, error: {}",
                        requestPath, e.getMessage());
                SecurityContextHolder.clearContext();
            }
        }

        filterChain.doFilter(request, response);
    }

    private String resolveToken(HttpServletRequest request) {
        String bearer = request.getHeader("Authorization");
        if (StringUtils.hasText(bearer) && bearer.startsWith("Bearer ")) {
            return bearer.substring(7);
        }
        return null;
    }
}
