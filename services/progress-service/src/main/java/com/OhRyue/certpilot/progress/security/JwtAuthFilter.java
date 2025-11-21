package com.OhRyue.certpilot.progress.security;

import com.OhRyue.common.web.JwtUtil;
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

    private final JwtUtil jwtUtil;

    public JwtAuthFilter(
            @Value("${auth.jwt.secret:${jwt.secret-key:}}") String secret // 새/기존 둘 다 지원
    ) {
        this.jwtUtil = new JwtUtil(secret);
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        String path = request.getRequestURI();

        // Swagger / actuator 등 공개 경로는 통과
        if (isPublicPath(path)) {
            filterChain.doFilter(request, response);
            return;
        }

        String userIdHeader = request.getHeader("X-User-Id");
        String rolesHeader = request.getHeader("X-User-Roles");
        String authHeader = request.getHeader("Authorization");

        String userId = null;
        String[] roles = new String[0];

        try {
            if (StringUtils.hasText(userIdHeader)) {
                userId = userIdHeader;
                if (StringUtils.hasText(rolesHeader)) {
                    roles = rolesHeader.split(",");
                }
            } else if (StringUtils.hasText(authHeader) && authHeader.startsWith("Bearer ")) {
                userId = jwtUtil.getUserId(authHeader);
                roles = jwtUtil.getRoles(authHeader);
            }

            if (userId != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                var authorities = Arrays.stream(roles)
                        .filter(StringUtils::hasText)
                        .map(r -> r.startsWith("ROLE_") ? r : "ROLE_" + r)
                        .map(SimpleGrantedAuthority::new)
                        .collect(Collectors.toList());

                var auth = new UsernamePasswordAuthenticationToken(
                        userId, null, authorities
                );
                auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(auth);

                log.debug("[progress-service] ✅ JWT 인증 성공 - userId: {}, roles: {}, path: {}",
                        userId, Arrays.toString(roles), path);
            }

        } catch (Exception e) {
            log.warn("[progress-service] ❌ JWT 인증 실패 - path: {}, error: {}", path, e.getMessage());
            SecurityContextHolder.clearContext();
        }

        filterChain.doFilter(request, response);
    }

    private boolean isPublicPath(String path) {
        return path.startsWith("/actuator")
                || path.startsWith("/v3/api-docs")
                || path.startsWith("/swagger-ui")
                || path.startsWith("/swagger-ui.html");
    }
}
