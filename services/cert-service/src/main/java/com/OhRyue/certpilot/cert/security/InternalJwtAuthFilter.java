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
import java.util.List;

/**
 * Internal JWT 인증 필터
 * 
 * study-service 등 내부 서비스에서 cert-service를 호출할 때 사용하는 Internal JWT를 검증합니다.
 * 
 * Internal JWT 특징:
 * - sub: internal-study-service
 * - roles: ["INTERNAL"]
 * - iss: certpilot-internal
 * - aud: cert-service
 * - alg: HS256
 * 
 * 주의: INTERNAL_JWT_SECRET 환경변수가 설정되지 않으면 필터가 비활성화됩니다.
 */
@Component
public class InternalJwtAuthFilter extends OncePerRequestFilter {

    private static final Logger log = LoggerFactory.getLogger(InternalJwtAuthFilter.class);
    private static final String ROLE_INTERNAL = "ROLE_INTERNAL";
    
    // Internal JWT가 필요한 경로 패턴
    private static final String[] INTERNAL_PATHS = {
        "/api/cert/internal"  // 기타 internal API
        // 참고: /api/cert/topics는 외부 공개 API이므로 제외됨
    };

    private final JwtUtil internalJwtUtil;
    private final boolean enabled;
    private final String expectedIssuer;
    private final String expectedAudience;

    public InternalJwtAuthFilter(
            @Value("${auth.internal.jwt.secret:}") String secret,
            @Value("${auth.internal.jwt.issuer:certpilot-internal}") String issuer,
            @Value("${auth.internal.jwt.audience:cert-service}") String audience
    ) {
        if (secret == null || secret.isBlank()) {
            log.warn("[cert-service] ⚠️ auth.internal.jwt.secret이 설정되지 않았습니다. InternalJwtAuthFilter가 비활성화됩니다.");
            log.warn("[cert-service] ⚠️ INTERNAL_JWT_SECRET 환경변수를 설정하면 Internal JWT 인증이 활성화됩니다.");
            this.internalJwtUtil = null;
            this.enabled = false;
            this.expectedIssuer = null;
            this.expectedAudience = null;
        } else {
            this.internalJwtUtil = new JwtUtil(secret);
            this.enabled = true;
            this.expectedIssuer = issuer;
            this.expectedAudience = audience;
            log.info("[cert-service] ✅ InternalJwtAuthFilter initialized: issuer={}, audience={}", issuer, audience);
        }
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getRequestURI();
        
        // Internal JWT가 비활성화된 경우 필터링 스킵
        if (!enabled) {
            log.debug("[cert-service] InternalJwtAuthFilter skipped (disabled): path={}", path);
            return true;
        }
        
        // 외부 공개 API는 필터링 스킵
        if (path.equals("/api/cert/topics") || path.startsWith("/api/cert/topics?")) {
            log.debug("[cert-service] InternalJwtAuthFilter skipped (public API): path={}", path);
            return true;
        }
        
        // Internal JWT가 필요한 경로인지 확인
        for (String internalPath : INTERNAL_PATHS) {
            if (path.startsWith(internalPath)) {
                log.debug("[cert-service] InternalJwtAuthFilter will process: path={}, internalPath={}", path, internalPath);
                return false; // 필터링 필요
            }
        }
        
        log.debug("[cert-service] InternalJwtAuthFilter skipped (not internal path): path={}", path);
        return true; // 필터링 불필요
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain chain) throws ServletException, IOException {

        String path = request.getRequestURI();
        log.info("[cert-service] 🔍 InternalJwtAuthFilter processing: path={}, method={}", path, request.getMethod());
        
        // Internal JWT가 비활성화된 경우 통과
        if (!enabled || internalJwtUtil == null) {
            log.warn("[cert-service] ⚠️ InternalJwtAuthFilter가 비활성화되어 있습니다. INTERNAL_JWT_SECRET 환경변수를 설정해주세요.");
            chain.doFilter(request, response);
            return;
        }

        // 이미 인증된 경우 스킵
        if (SecurityContextHolder.getContext().getAuthentication() != null) {
            log.debug("[cert-service] InternalJwtAuthFilter skipped (already authenticated): path={}", path);
            chain.doFilter(request, response);
            return;
        }

        String authHeader = request.getHeader("Authorization");
        log.debug("[cert-service] InternalJwtAuthFilter: Authorization header present={}, startsWith Bearer={}", 
                authHeader != null, authHeader != null && authHeader.startsWith("Bearer "));
        
        if (!StringUtils.hasText(authHeader) || !authHeader.startsWith("Bearer ")) {
            log.error("[cert-service] ❌ Internal JWT missing: path={}, Authorization header={}", 
                    path, authHeader != null ? "present (but not Bearer)" : "missing");
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, 
                    "Internal JWT required for internal API calls");
            return;
        }

        String token = authHeader.substring(7);
        log.debug("[cert-service] InternalJwtAuthFilter: token extracted, length={}", token.length());
        
        try {
            log.debug("[cert-service] InternalJwtAuthFilter: starting token validation");
            
            // 토큰 만료 확인 (먼저 수행하여 만료된 토큰은 즉시 거부)
            if (internalJwtUtil.isExpired(token)) {
                log.error("[cert-service] ❌ Internal JWT expired: path={}", path);
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT expired");
                return;
            }
            log.debug("[cert-service] InternalJwtAuthFilter: token expiration check passed");

            // 사용자 ID 추출 (sub 클레임) - 이 과정에서 서명 검증이 수행됨
            String userId;
            try {
                userId = internalJwtUtil.getUserId(token);
                log.debug("[cert-service] InternalJwtAuthFilter: userId extracted={}, signature validation passed", userId);
            } catch (io.jsonwebtoken.security.SignatureException e) {
                log.error("[cert-service] ❌ Internal JWT signature validation failed: path={}, error={}", 
                        path, e.getMessage());
                log.error("[cert-service] ❌ 원인: INTERNAL_JWT_SECRET 불일치 또는 토큰 변조");
                throw e; // 아래 catch 블록에서 처리
            }
            
            if (userId == null || !userId.startsWith("internal-")) {
                log.error("[cert-service] ❌ Internal JWT invalid subject: path={}, userId={}", path, userId);
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT invalid subject");
                return;
            }

            // 토큰을 파싱하여 issuer/audience 확인 (서명 검증은 이미 getUserId()에서 수행됨)
            // JwtUtil의 getClaims() 메서드 사용 (서명 검증 포함, 하지만 이미 검증되었으므로 재검증)
            io.jsonwebtoken.Claims claims = internalJwtUtil.getClaims(token);
            log.debug("[cert-service] InternalJwtAuthFilter: claims extracted successfully");
            
            // Issuer 검증
            String issuer = claims.getIssuer();
            log.debug("[cert-service] InternalJwtAuthFilter: issuer check - expected={}, actual={}", expectedIssuer, issuer);
            if (issuer == null || !expectedIssuer.equals(issuer)) {
                log.error("[cert-service] ❌ Internal JWT issuer mismatch: path={}, expected={}, actual={}", 
                        path, expectedIssuer, issuer);
                log.error("[cert-service] ❌ 원인: study-service의 INTERNAL_JWT_ISSUER와 cert-service의 auth.internal.jwt.issuer 불일치");
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, 
                        String.format("Internal JWT issuer mismatch. expected=%s, actual=%s", expectedIssuer, issuer));
                return;
            }
            log.debug("[cert-service] InternalJwtAuthFilter: issuer validation passed");

            // Audience 검증
            Object audObj = claims.get("aud");
            String audience = null;
            if (audObj instanceof String) {
                audience = (String) audObj;
            } else if (audObj instanceof java.util.List) {
                @SuppressWarnings("unchecked")
                java.util.List<String> audList = (java.util.List<String>) audObj;
                if (!audList.isEmpty()) {
                    audience = audList.get(0);
                }
            }
            
            log.debug("[cert-service] InternalJwtAuthFilter: audience check - expected={}, actual={}", expectedAudience, audience);
            if (audience == null || !expectedAudience.equals(audience)) {
                log.error("[cert-service] ❌ Internal JWT audience mismatch: path={}, expected={}, actual={}", 
                        path, expectedAudience, audience);
                log.error("[cert-service] ❌ 원인: study-service가 생성한 토큰의 audience와 cert-service의 auth.internal.jwt.audience 불일치");
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, 
                        String.format("Internal JWT audience mismatch. expected=%s, actual=%s", expectedAudience, audience));
                return;
            }
            log.debug("[cert-service] InternalJwtAuthFilter: audience validation passed");

            // 역할 확인 (INTERNAL 역할이 있어야 함)
            // InternalTokenProvider는 roles를 List.of("INTERNAL")로 저장하므로, 직접 파싱 필요
            boolean hasInternalRole = false;
            Object rolesClaim = claims.get("roles");
            
            log.debug("[cert-service] InternalJwtAuthFilter: roles claim type={}, value={}", 
                    rolesClaim != null ? rolesClaim.getClass().getName() : "null", rolesClaim);
            
            if (rolesClaim != null) {
                if (rolesClaim instanceof java.util.List) {
                    // List 타입 (InternalTokenProvider가 List.of("INTERNAL")로 저장)
                    @SuppressWarnings("unchecked")
                    java.util.List<Object> rolesList = (java.util.List<Object>) rolesClaim;
                    for (Object roleObj : rolesList) {
                        String role = roleObj != null ? roleObj.toString() : null;
                        log.debug("[cert-service] InternalJwtAuthFilter: checking role from list: {}", role);
                        if (role != null && ("INTERNAL".equalsIgnoreCase(role) || "ROLE_INTERNAL".equalsIgnoreCase(role))) {
                            hasInternalRole = true;
                            log.debug("[cert-service] InternalJwtAuthFilter: INTERNAL role found in list");
                            break;
                        }
                    }
                } else if (rolesClaim instanceof String) {
                    // String 타입 (쉼표로 구분된 경우)
                    String rolesStr = (String) rolesClaim;
                    log.debug("[cert-service] InternalJwtAuthFilter: checking role from string: {}", rolesStr);
                    String[] roles = rolesStr.split(",");
                    for (String role : roles) {
                        role = role.trim();
                        if ("INTERNAL".equalsIgnoreCase(role) || "ROLE_INTERNAL".equalsIgnoreCase(role)) {
                            hasInternalRole = true;
                            log.debug("[cert-service] InternalJwtAuthFilter: INTERNAL role found in string");
                            break;
                        }
                    }
                } else {
                    // 기타 타입 (toString()으로 변환 후 처리)
                    String rolesStr = rolesClaim.toString();
                    log.debug("[cert-service] InternalJwtAuthFilter: checking role from toString: {}", rolesStr);
                    if (rolesStr.contains("INTERNAL") || rolesStr.contains("ROLE_INTERNAL")) {
                        hasInternalRole = true;
                        log.debug("[cert-service] InternalJwtAuthFilter: INTERNAL role found in toString");
                    }
                }
            }
            
            // JwtUtil.getRoles()도 시도 (fallback)
            if (!hasInternalRole) {
                try {
                    String[] roles = internalJwtUtil.getRoles(token);
                    log.debug("[cert-service] InternalJwtAuthFilter: fallback to JwtUtil.getRoles(), result={}", 
                            roles != null ? java.util.Arrays.toString(roles) : "null");
                    if (roles != null) {
                        for (String role : roles) {
                            if (role != null && ("INTERNAL".equalsIgnoreCase(role) || "ROLE_INTERNAL".equalsIgnoreCase(role))) {
                                hasInternalRole = true;
                                log.debug("[cert-service] InternalJwtAuthFilter: INTERNAL role found via JwtUtil");
                                break;
                            }
                        }
                    }
                } catch (Exception e) {
                    log.debug("[cert-service] InternalJwtAuthFilter: JwtUtil.getRoles() failed: {}", e.getMessage());
                }
            }
            
            if (!hasInternalRole) {
                log.error("[cert-service] ❌ Internal JWT missing INTERNAL role: path={}, rolesClaim={}, rolesClaimType={}", 
                        path, rolesClaim, rolesClaim != null ? rolesClaim.getClass().getName() : "null");
                log.error("[cert-service] ❌ 원인: 토큰의 roles 클레임에 INTERNAL 또는 ROLE_INTERNAL이 없음");
                log.error("[cert-service] ❌ 기대값: roles 클레임에 \"INTERNAL\" 또는 \"ROLE_INTERNAL\" 포함 필요");
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT missing INTERNAL role");
                return;
            }
            
            log.debug("[cert-service] InternalJwtAuthFilter: INTERNAL role validation passed");
            
            // 디버그 로깅: 토큰 검증 성공 정보
            String rolesStr = rolesClaim != null ? rolesClaim.toString() : "null";
            log.debug("[cert-service] ✅ Internal JWT 검증 통과: path={}, sub={}, iss={}, aud={}, roles={}", 
                    path, userId, issuer, audience, rolesStr);

            // 인증 성공: SecurityContext에 설정
            var authToken = new UsernamePasswordAuthenticationToken(
                    userId,
                    null,
                    List.of(new SimpleGrantedAuthority(ROLE_INTERNAL))
            );
            authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
            SecurityContextHolder.getContext().setAuthentication(authToken);

            log.info("[cert-service] ✅ Internal JWT 인증 성공: path={}, userId={}", path, userId);
            chain.doFilter(request, response);

        } catch (io.jsonwebtoken.security.SignatureException e) {
            // 서명 불일치 (가장 흔한 원인)
            log.error("[cert-service] ❌ Internal JWT signature mismatch: path={}, error={}", 
                    path, e.getMessage());
            log.error("[cert-service] ❌ 원인 분석: INTERNAL_JWT_SECRET이 study-service와 cert-service 간 불일치 가능성");
            log.error("[cert-service] ❌ 해결 방법: study-service와 cert-service의 INTERNAL_JWT_SECRET 환경변수가 동일한지 확인");
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, 
                    "Internal JWT signature mismatch. Check INTERNAL_JWT_SECRET consistency.");
            
        } catch (io.jsonwebtoken.ExpiredJwtException e) {
            log.warn("[cert-service] ❌ Internal JWT expired: path={}, exp={}, now={}", 
                    path, e.getClaims().getExpiration(), new java.util.Date());
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT expired");
            
        } catch (io.jsonwebtoken.MalformedJwtException e) {
            log.error("[cert-service] ❌ Internal JWT malformed: path={}, error={}", path, e.getMessage());
            log.error("[cert-service] ❌ 원인 분석: 토큰 형식이 올바르지 않음 (헤더/페이로드/서명 구조 오류)");
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT malformed");
            
        } catch (io.jsonwebtoken.UnsupportedJwtException e) {
            log.error("[cert-service] ❌ Internal JWT unsupported: path={}, error={}", path, e.getMessage());
            log.error("[cert-service] ❌ 원인 분석: 지원하지 않는 JWT 형식 또는 알고리즘");
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT unsupported");
            
        } catch (io.jsonwebtoken.security.InvalidKeyException e) {
            log.error("[cert-service] ❌ Internal JWT invalid key: path={}, error={}", path, e.getMessage());
            log.error("[cert-service] ❌ 원인 분석: INTERNAL_JWT_SECRET이 올바르지 않거나 키 생성 실패");
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT invalid key");
            
        } catch (Exception e) {
            log.error("[cert-service] ❌ Internal JWT 인증/파싱 실패: path={}, error={}, class={}", 
                    path, e.getMessage(), e.getClass().getName(), e);
            log.error("[cert-service] ❌ 원인 분석: 예상치 못한 예외 발생");
            SecurityContextHolder.clearContext();
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Internal JWT invalid");
        }
    }
    
}

