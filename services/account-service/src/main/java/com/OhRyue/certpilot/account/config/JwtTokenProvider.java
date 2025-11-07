package com.OhRyue.certpilot.account.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.security.Key;
import java.util.Date;

@Component
public class JwtTokenProvider {

  // application.yml에서 설정한 jwt.secret-key 시크릿 키 주입
  @Value("${jwt.secret-key}")
  private String secretKey;

  private Key key; // 실제 서명에 사용할 Key 객체
  private final long validityInMs = 1000L * 60 * 60; // 1시간

  // 애플리케이션 실행 시 secretKey를 Base64로 인코딩
  @PostConstruct
  protected void init() {
    // 1) Base64 인코딩
    secretKey = Base64.getEncoder().encodeToString(secretKey.getBytes());
    // 2) HMAC-SHA 키 객체 생성
    this.key = Keys.hmacShaKeyFor(secretKey.getBytes());
  }

  // 토큰 생성 (로그인 성공 시 사용)
  public String generateToken(String username) {
    long now = System.currentTimeMillis();
    Date issuedAt = new Date(now);
    Date expiryAt = new Date(now + validityInMs);

    return Jwts.builder()
        .setSubject(username)                       // 토큰 주체 (유저 이름=PK)
        .claim("role", "USER")                      // 스키마에 role 없음 → 기본 USER
        .setIssuedAt(issuedAt)                      // 발급 시간
        .setExpiration(expiryAt)                    // 만료 시간
        .signWith(key, SignatureAlgorithm.HS256)    // 서명
        .compact();
  }

  // Refresh Token 생성
  public String generateRefreshToken(String username) {
    long now = System.currentTimeMillis();
    Date issuedAt = new Date(now);
    Date expiryAt = new Date(now + 1000L * 60 * 60 * 24 * 7); // 7일

    return Jwts.builder()
        .setSubject(username)
        .claim("role", "USER")
        .setIssuedAt(issuedAt)
        .setExpiration(expiryAt)
        .signWith(key, SignatureAlgorithm.HS256)
        .compact();
  }

  // 토큰에서 username 추출
  public String getUsernameFromToken(String token) {
    return Jwts.parserBuilder()
        .setSigningKey(key)
        .build()
        .parseClaimsJws(token)
        .getBody()
        .getSubject();
  }

  // 토큰에서 role 추출
  public String getRoleFromToken(String token) {
    return (String) Jwts.parserBuilder()
        .setSigningKey(key)
        .build()
        .parseClaimsJws(token)
        .getBody()
        .get("role");
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

  // 스프링 시큐리티 인증 객체에 필요한 권한(Authority) 생성
  public Collection<? extends GrantedAuthority> getAuthorities(String role) {
    return List.of(new SimpleGrantedAuthority("ROLE_" + role));
  }
}
