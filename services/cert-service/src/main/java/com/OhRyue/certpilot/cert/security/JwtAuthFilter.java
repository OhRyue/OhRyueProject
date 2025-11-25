package com.OhRyue.certpilot.cert.security;

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

    private final JwtUtil jwtUtil;

    public JwtAuthFilter(@Value("${auth.jwt.secret}") String secret) {
        this.jwtUtil = new JwtUtil(secret);
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        String path = request.getRequestURI();

        // Swagger, actuator 등 공개 경로는 그냥 통과
        if (isPublicPath(path)) {
            filterChain.doFilter(request, response);
            return;
        }

        // 1) Gateway가 붙여준 헤더 우선 사용
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
                // 혹시 게이트웨이 우회 호출 대비해서 JWT 직접 파싱
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
                        userId,
                        null,
                        authorities
                );
                auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(auth);

                log.debug("[cert-service] ✅ JWT 인증 성공 - userId: {}, roles: {}, path: {}",
                        userId, Arrays.toString(roles), path);
            }

        } catch (Exception e) {
            log.warn("[cert-service] ❌ JWT 인증/파싱 실패 - path: {}, error: {}",
                    path, e.getMessage());
            SecurityContextHolder.clearContext();
        }

        filterChain.doFilter(request, response);
    }

    private boolean isPublicPath(String path) {
        return path.startsWith("/actuator")
                || path.startsWith("/v3/api-docs")
                || path.startsWith("/swagger-ui")
                || path.startsWith("/swagger-ui.html")
                || path.startsWith("/api/cert/external");  // Q-Net External API는 공개 경로
    }
}
