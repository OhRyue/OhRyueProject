package com.OhRyue.certpilot.versus.websocket;

import com.OhRyue.common.security.JwtUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;

import java.security.Principal;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * WebSocket 연결 시 JWT 인증을 수행하는 HandshakeInterceptor
 * 
 * 동작 방식:
 * 1. WebSocket Handshake 요청에서 JWT 토큰을 추출
 *    - Query parameter: ?token=xxx
 *    - Header: Authorization: Bearer xxx
 * 
 * 2. JWT 토큰 검증 후 userId 및 roles 추출
 * 
 * 3. 인증 성공 시:
 *    - userId를 WebSocket 세션의 attributes에 저장
 *    - SecurityContext에 Authentication 설정 (AuthUserUtil과 호환)
 * 
 * 4. 인증 실패 시:
 *    - Handshake 거부 (HTTP 403)
 * 
 * AuthUserUtil 호환성:
 * - SecurityContext에 Authentication을 설정하므로
 * - 기존 AuthUserUtil.getCurrentUserId() 등이 정상 동작
 */
@Component
public class JwtHandshakeInterceptor implements HandshakeInterceptor {

    private static final Logger log = LoggerFactory.getLogger(JwtHandshakeInterceptor.class);
    private static final String USER_ID_ATTRIBUTE = "userId";
    private static final String TOKEN_PARAM = "token";

    private final JwtUtil jwtUtil;

    public JwtHandshakeInterceptor(
            @Value("${auth.jwt.secret:${jwt.secret-key:}}") String secret
    ) {
        this.jwtUtil = new JwtUtil(secret);
    }

    @Override
    public boolean beforeHandshake(
            @NonNull ServerHttpRequest request,
            @NonNull ServerHttpResponse response,
            @NonNull WebSocketHandler wsHandler,
            @NonNull Map<String, Object> attributes
    ) throws Exception {
        try {
            // Servlet 기반 요청인 경우에만 처리
            if (!(request instanceof ServletServerHttpRequest servletRequest)) {
                log.warn("WebSocket handshake: Not a Servlet request");
                return false;
            }

            var httpRequest = servletRequest.getServletRequest();
            String token = extractToken(httpRequest);

            if (!StringUtils.hasText(token)) {
                log.warn("WebSocket handshake: No token provided");
                response.setStatusCode(org.springframework.http.HttpStatus.UNAUTHORIZED);
                return false;
            }

            // JWT 검증 및 userId/roles 추출
            String userId;
            String[] roles;

            try {
                userId = jwtUtil.getUserId(token);
                roles = jwtUtil.getRoles(token);

                if (!StringUtils.hasText(userId)) {
                    log.warn("WebSocket handshake: Invalid token - no userId");
                    response.setStatusCode(org.springframework.http.HttpStatus.UNAUTHORIZED);
                    return false;
                }

                // 만료 체크
                if (jwtUtil.isExpired(token)) {
                    log.warn("WebSocket handshake: Token expired - userId: {}", userId);
                    response.setStatusCode(org.springframework.http.HttpStatus.UNAUTHORIZED);
                    return false;
                }

            } catch (Exception e) {
                log.warn("WebSocket handshake: Token validation failed - {}", e.getMessage());
                response.setStatusCode(org.springframework.http.HttpStatus.UNAUTHORIZED);
                return false;
            }

            // WebSocket 세션 attributes에 userId 저장
            attributes.put(USER_ID_ATTRIBUTE, userId);

            // SecurityContext에 Authentication 설정 (AuthUserUtil 호환)
            var authorities = Arrays.stream(roles)
                    .filter(StringUtils::hasText)
                    .map(r -> r.startsWith("ROLE_") ? r : "ROLE_" + r)
                    .map(SimpleGrantedAuthority::new)
                    .collect(Collectors.toList());

            var authentication = new UsernamePasswordAuthenticationToken(
                    userId, null, authorities
            );
            SecurityContextHolder.getContext().setAuthentication(authentication);

            log.info("WebSocket handshake: ✅ JWT 인증 성공 - userId: {}, roles: {}",
                    userId, Arrays.toString(roles));

            return true;

        } catch (Exception e) {
            log.error("WebSocket handshake: Unexpected error", e);
            response.setStatusCode(org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR);
            return false;
        }
    }

    @Override
    public void afterHandshake(
            @NonNull ServerHttpRequest request,
            @NonNull ServerHttpResponse response,
            @NonNull WebSocketHandler wsHandler,
            Exception exception
    ) {
        // Handshake 완료 후 처리 (필요 시)
        if (exception != null) {
            log.error("WebSocket handshake: After handshake error", exception);
        }
    }

    /**
     * HTTP 요청에서 JWT 토큰 추출
     * 우선순위:
     * 1. Query parameter: ?token=xxx
     * 2. Authorization header: Bearer xxx
     */
    private String extractToken(jakarta.servlet.http.HttpServletRequest request) {
        // Query parameter에서 추출
        String token = request.getParameter(TOKEN_PARAM);
        if (StringUtils.hasText(token)) {
            return token;
        }

        // Authorization header에서 추출
        String authHeader = request.getHeader("Authorization");
        if (StringUtils.hasText(authHeader) && authHeader.startsWith("Bearer ")) {
            return authHeader.substring(7);
        }

        return null;
    }

    /**
     * WebSocket 세션에서 userId 추출 헬퍼 메서드
     * (Controller에서 사용 가능)
     */
    public static String getUserIdFromAttributes(Map<String, Object> attributes) {
        Object userId = attributes.get(USER_ID_ATTRIBUTE);
        return userId != null ? userId.toString() : null;
    }

    /**
     * Principal에서 userId 추출 헬퍼 메서드
     * (STOMP 메시지 핸들러에서 사용 가능)
     */
    public static String getUserIdFromPrincipal(Principal principal) {
        if (principal == null) {
            return null;
        }
        // Principal의 name이 userId로 설정됨
        return principal.getName();
    }
}


