package com.OhRyue.common.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.util.Date;

/**
 * 공통 JWT 유틸리티.
 * - sub(Subject)에 userId 저장
 * - roles 또는 role 클레임에서 권한 문자열 읽기
 */
public class JwtUtil {

    private final Key key;

    public JwtUtil(String secret) {
        this.key = Keys.hmacShaKeyFor(secret.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * JWT에서 userId(subject) 추출
     */
    public String getUserId(String token) {
        return getAllClaims(token).getSubject();
    }

    /**
     * JWT에서 roles / role 클레임을 읽어서 배열로 반환
     * - roles: "ROLE_USER,ROLE_ADMIN"
     * - role:  "ROLE_USER"
     */
    public String[] getRoles(String token) {
        Claims claims = getAllClaims(token);

        Object rolesClaim = claims.get("roles");
        if (rolesClaim == null) {
            rolesClaim = claims.get("role");
        }
        if (rolesClaim == null) {
            return new String[0];
        }

        String value = rolesClaim.toString();
        if (value.isBlank()) {
            return new String[0];
        }
        return value.split(",");
    }

    /**
     * 만료 여부 확인
     */
    public boolean isExpired(String token) {
        Date exp = getAllClaims(token).getExpiration();
        return exp != null && exp.before(new Date());
    }

    private Claims getAllClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(key)
                .build()
                .parseClaimsJws(stripBearer(token))
                .getBody();
    }

    private String stripBearer(String token) {
        if (token == null) return null;
        if (token.startsWith("Bearer ")) {
            return token.substring(7);
        }
        return token;
    }
}
