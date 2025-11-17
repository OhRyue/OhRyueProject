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
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private static final Logger log = LoggerFactory.getLogger(JwtAuthenticationFilter.class);
    private final JwtTokenProvider jwtTokenProvider;

    public JwtAuthenticationFilter(JwtTokenProvider jwtTokenProvider) {
        this.jwtTokenProvider = jwtTokenProvider;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        String requestPath = request.getRequestURI();
        
        // 1. Authorization 헤더에서 JWT 가져오기
        String token = resolveToken(request);

        if (token == null) {
            log.info("🔍 JWT 토큰 없음 - 경로: {}", requestPath);
        } else {
            log.info("🔍 JWT 토큰 발견 - 경로: {}, 토큰 앞 20자: {}", 
                requestPath, token.length() > 20 ? token.substring(0, 20) + "..." : token);
        }

        // 2. 유효한 토큰인지 확인
        if (token != null && jwtTokenProvider.validateToken(token)) {
            try {
                // 3. 토큰에서 userId, role 가져오기
                String userId = jwtTokenProvider.getUsernameFromToken(token);
                String role = jwtTokenProvider.getRoleFromToken(token);

                log.info("✅ JWT 인증 성공 - userId: {}, role: {}, 경로: {}", userId, role, requestPath);

                // 4. 시큐리티 인증 객체 만들기
                UsernamePasswordAuthenticationToken auth =
                        new UsernamePasswordAuthenticationToken(
                                userId,           // principal: userId
                                null,             // credentials (우리는 비밀번호 안 씀)
                                jwtTokenProvider.getAuthorities(role) // ROLE_USER 등
                        );
                auth.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                // 5. 시큐리티 컨텍스트에 저장
                SecurityContextHolder.getContext().setAuthentication(auth);
            } catch (Exception e) {
                log.error("❌ JWT 토큰에서 사용자 정보 추출 실패 - 경로: {}, 오류: {}", 
                    requestPath, e.getMessage(), e);
            }
        } else if (token != null) {
            log.warn("❌ JWT 토큰 검증 실패 - 경로: {}", requestPath);
        }

        // 다음 필터로 넘김
        filterChain.doFilter(request, response);
    }

    // 헤더에서 토큰 분리
    private String resolveToken(HttpServletRequest request) {
        String bearer = request.getHeader("Authorization");
        if (StringUtils.hasText(bearer) && bearer.startsWith("Bearer ")) {
            return bearer.substring(7);
        }
        return null;
    }
}
