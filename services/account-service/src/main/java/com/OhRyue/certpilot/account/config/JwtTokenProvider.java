package com.OhRyue.certpilot.account.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Base64;
import java.util.Collection;
import java.util.Date;
import java.util.List;

@Component
public class JwtTokenProvider {

    private static final Logger log = LoggerFactory.getLogger(JwtTokenProvider.class);

    /**
     * 프로젝트 공통 JWT 시크릿.
     * - 신규: auth.jwt.secret
     * - 구버전: jwt.secret-key (fallback)
     */
    @Value("${auth.jwt.secret:${jwt.secret-key:}}")
    private String secretKey;

    private String originalSecretKey; // 원본 secret key (로깅용)
    private Key key;                  // 실제 서명에 사용할 Key 객체

    // Access Token 유효기간: 1시간
    private final long validityInMs = 1000L * 60 * 60;

    @PostConstruct
    protected void init() {
        if (secretKey == null || secretKey.isBlank()) {
            throw new IllegalStateException(
                    "JWT secret key가 설정되지 않았습니다. " +
                            "auth.jwt.secret 또는 jwt.secret-key 를 설정하세요."
            );
        }

        this.originalSecretKey = secretKey;

        // JwtUtil 과 완전히 동일한 방식으로 키 생성 (Base64 디코딩 포함)
        byte[] keyBytes = decodeSecret(secretKey);
        if (keyBytes.length < 32) {
            // JwtUtil 도 동일하게 32바이트 미만이면 IllegalArgumentException 를 던지므로,
            // 여기서도 명시적으로 막아줍니다.
            throw new IllegalStateException(
                    "JWT secret key 길이가 32바이트 미만입니다. " +
                            "보안을 위해 최소 32바이트 이상(가능하면 더 길게)의 시크릿을 사용하세요. " +
                            "현재 길이: " + keyBytes.length + " bytes"
            );
        }

        this.key = Keys.hmacShaKeyFor(keyBytes);

        log.info("🔑 JwtTokenProvider 초기화 완료 - 알고리즘: HS256, Secret Key 길이: {} bytes (원본 문자열 길이: {} chars)",
                keyBytes.length, originalSecretKey.length());
    }

    /**
     * Secret을 디코딩합니다.
     * - Base64 문자열인 경우 디코딩
     * - 그 외의 경우 UTF-8 bytes로 변환
     */
    private byte[] decodeSecret(String secret) {
        try {
            // Base64 디코딩 시도
            byte[] decoded = Base64.getDecoder().decode(secret);
            log.debug("🔓 Base64 디코딩 성공 - 원본 길이: {} chars, 디코딩 후: {} bytes", secret.length(), decoded.length);
            return decoded;
        } catch (IllegalArgumentException e) {
            // Base64가 아니면 raw string으로 처리
            log.debug("🔓 Base64 디코딩 실패 → raw string으로 처리 - 길이: {} bytes", secret.getBytes(StandardCharsets.UTF_8).length);
            return secret.getBytes(StandardCharsets.UTF_8);
        }
    }

    // ------------------------------------------------------------------------
    // Token 생성
    // ------------------------------------------------------------------------

    // Access Token 생성 (로그인 성공 시 사용)
    public String generateToken(String userId) {
        long now = System.currentTimeMillis();
        Date issuedAt = new Date(now);
        Date expiryAt = new Date(now + validityInMs);

        String token = Jwts.builder()
                .setSubject(userId)               // 토큰 주체 (userId)
                .claim("role", "USER")            // 기본 role
                .setIssuedAt(issuedAt)            // 발급 시간
                .setExpiration(expiryAt)          // 만료 시간
                .signWith(key, SignatureAlgorithm.HS256)
                .compact();

        log.info("✅ Access Token 발급 완료 - userId: {}, 만료시간: {}", userId, expiryAt);
        return token;
    }

    // Refresh Token 생성
    public String generateRefreshToken(String userId) {
        long now = System.currentTimeMillis();
        Date issuedAt = new Date(now);
        Date expiryAt = new Date(now + 1000L * 60 * 60 * 24 * 7); // 7일

        String token = Jwts.builder()
                .setSubject(userId)
                .claim("role", "USER")
                .setIssuedAt(issuedAt)
                .setExpiration(expiryAt)
                .signWith(key, SignatureAlgorithm.HS256)
                .compact();

        log.info("✅ Refresh Token 발급 완료 - userId: {}, 만료시간: {}", userId, expiryAt);
        return token;
    }

    // ------------------------------------------------------------------------
    // Token 파싱 / 검증
    // ------------------------------------------------------------------------

    // Bearer prefix 제거 헬퍼
    private String stripBearer(String token) {
        if (token == null) return null;
        if (token.startsWith("Bearer ")) {
            return token.substring(7);
        }
        return token;
    }

    // Claims 공통 파싱
    private Claims parseClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(key)
                .build()
                .parseClaimsJws(stripBearer(token))
                .getBody();
    }

    // 토큰에서 userId(subject) 추출
    public String getUsernameFromToken(String token) {
        try {
            Claims claims = parseClaims(token);
            String userId = claims.getSubject();
            log.debug("📝 토큰에서 userId 추출 성공: {}", userId);
            return userId;
        } catch (Exception e) {
            log.error("❌ 토큰에서 userId 추출 실패 - 오류: {}", e.getMessage(), e);
            throw e;
        }
    }

    // 토큰에서 role 추출 (단일 role)
    public String getRoleFromToken(String token) {
        try {
            Claims claims = parseClaims(token);
            String role = claims.get("role", String.class);
            log.debug("📝 토큰에서 role 추출 성공: {}", role);
            return role;
        } catch (Exception e) {
            log.error("❌ 토큰에서 role 추출 실패 - 오류: {}", e.getMessage(), e);
            throw e;
        }
    }

    // 토큰 유효성 검사
    public boolean validateToken(String token) {
        if (token == null || token.trim().isEmpty()) {
            log.warn("❌ JWT 토큰이 null이거나 비어있습니다");
            return false;
        }

        try {
            Claims claims = parseClaims(token);
            String userId = claims.getSubject();
            log.info("✅ JWT 토큰 검증 성공 - userId: {}, 알고리즘: HS256", userId);
            return true;

        } catch (SecurityException | MalformedJwtException e) {
            log.error("❌ JWT 서명 오류 또는 잘못된 토큰 형식 - 오류: {}, 토큰 앞 20자: {}",
                    e.getMessage(),
                    token.length() > 20 ? token.substring(0, 20) + "..." : token);
            log.error("❌ 사용된 Secret Key 길이: {} bytes",
                    originalSecretKey != null ? originalSecretKey.getBytes(StandardCharsets.UTF_8).length : 0);

        } catch (ExpiredJwtException e) {
            log.warn("⏰ JWT 토큰 만료 - userId: {}, 만료시간: {}",
                    e.getClaims().getSubject(), e.getClaims().getExpiration());

        } catch (UnsupportedJwtException e) {
            log.error("❌ 지원하지 않는 JWT 형식 - 오류: {}", e.getMessage());

        } catch (IllegalArgumentException e) {
            log.error("❌ JWT 토큰이 비어있거나 잘못된 형식 - 오류: {}", e.getMessage());

        } catch (Exception e) {
            log.error("❌ JWT 검증 중 예상치 못한 오류 발생 - 타입: {}, 메시지: {}",
                    e.getClass().getSimpleName(), e.getMessage(), e);
        }
        return false;
    }

    // ------------------------------------------------------------------------
    // Spring Security 권한 매핑
    // ------------------------------------------------------------------------

    public Collection<? extends GrantedAuthority> getAuthorities(String role) {
        // role: "USER" → "ROLE_USER"
        return List.of(new SimpleGrantedAuthority("ROLE_" + role));
    }
}
