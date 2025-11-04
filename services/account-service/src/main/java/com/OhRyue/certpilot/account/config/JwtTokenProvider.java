package com.OhRyue.certpilot.account.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import org.springframework.stereotype.Component;

import java.security.Key;
import java.util.Date;

@Component
public class JwtTokenProvider {

    // JWT 서명에 사용할 key
    private final Key key = Keys.secretKeyFor(SignatureAlgorithm.HS256);

    // 토큰 생성 (로그인 성공 시 사용)
    public String generateToken(String username, String role) {
        long now = System.currentTimeMillis();
        long expireTime = now + 1000 * 60 * 60; // 1시간 유효

        return Jwts.builder()
                .setSubject(username)        // 토큰 주체 (유저 이름)
                .claim("role", role)        // 권한
                .setIssuedAt(new Date(now)) // 발급 시간
                .setExpiration(new Date(expireTime)) // 만료 시간
                .signWith(key)              // 서명
                .compact();
    }

    // 토큰에서 username 추출
    public String getUsernameFromToken(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(key).build()
                .parseClaimsJws(token)
                .getBody()
                .getSubject();
    }

    // 토큰 유효성 검사
    public boolean validateToken(String token) {
        try {
            Jwts.parserBuilder().setSigningKey(key).build().parseClaimsJws(token);
            return true;
        } catch (SecurityException | MalformedJwtException e) {
            System.out.println("🔴 JWT 서명 오류");
        } catch (ExpiredJwtException e) {
            System.out.println("🔴 JWT 만료");
        } catch (Exception e) {
            System.out.println("🔴 JWT 기타 오류");
        }
        return false;
    }
}
