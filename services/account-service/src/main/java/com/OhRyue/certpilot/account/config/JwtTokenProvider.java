package com.OhRyue.certpilot.account.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.security.Key;
import java.util.Date;

@Component
public class JwtTokenProvider {

  private static final Logger log = LoggerFactory.getLogger(JwtTokenProvider.class);

  // application.yml에서 설정한 jwt.secret-key 시크릿 키 주입
  @Value("${jwt.secret-key}")
  private String secretKey;

  private String originalSecretKey; // 원본 secret key (로깅용)
  private Key key; // 실제 서명에 사용할 Key 객체
  private final long validityInMs = 1000L * 60 * 60; // 1시간

  // 애플리케이션 실행 시 secretKey를 HMAC-SHA 키로 변환
  @PostConstruct
  protected void init() {
    // 1) 원본 secret key 저장 (로깅용)
    this.originalSecretKey = secretKey;
    
    // 2) 원본 secret key의 바이트 배열을 직접 사용하여 HMAC-SHA 키 생성
    // Base64 인코딩 없이 원본 바이트를 사용 (JJWT의 Keys.hmacShaKeyFor는 최소 32바이트 필요)
    byte[] keyBytes = secretKey.getBytes();
    
    // 3) HMAC-SHA 키 객체 생성 (최소 32바이트 필요, 부족하면 패딩)
    if (keyBytes.length < 32) {
      log.warn("⚠️ Secret Key가 32바이트보다 짧습니다. 보안을 위해 더 긴 키를 사용하세요.");
      // 32바이트로 패딩
      byte[] paddedKey = new byte[32];
      System.arraycopy(keyBytes, 0, paddedKey, 0, Math.min(keyBytes.length, 32));
      this.key = Keys.hmacShaKeyFor(paddedKey);
    } else {
      this.key = Keys.hmacShaKeyFor(keyBytes);
    }
    
    log.info("🔑 JWT TokenProvider 초기화 완료 - 알고리즘: HS256, Secret Key 길이: {} bytes (원본)", originalSecretKey.getBytes().length);
  }

  // 토큰 생성 (로그인 성공 시 사용)
  public String generateToken(String userId) {
    long now = System.currentTimeMillis();
    Date issuedAt = new Date(now);
    Date expiryAt = new Date(now + validityInMs);

    String token = Jwts.builder()
        .setSubject(userId)                       // 토큰 주체 (유저 이름=PK)
        .claim("role", "USER")                      // 스키마에 role 없음 → 기본 USER
        .setIssuedAt(issuedAt)                      // 발급 시간
        .setExpiration(expiryAt)                    // 만료 시간
        .signWith(key, SignatureAlgorithm.HS256)    // 서명
        .compact();
    
    log.info("✅ Access Token 발급 완료 - userId: {}, 알고리즘: HS256, 만료시간: {}", userId, expiryAt);
    return token;
  }

  // Refresh Token 생성
  public String generateRefreshToken(String userId) {
    long now = System.currentTimeMillis();
    Date issuedAt = new Date(now);
    Date expiryAt = new Date(now + 1000L * 60 * 60 * 24 * 7); // 7일

    return Jwts.builder()
        .setSubject(userId)
        .claim("role", "USER")
        .setIssuedAt(issuedAt)
        .setExpiration(expiryAt)
        .signWith(key, SignatureAlgorithm.HS256)
        .compact();
  }

  // 토큰에서 userId 추출
  public String getUsernameFromToken(String token) {
    try {
      String userId = Jwts.parserBuilder()
          .setSigningKey(key)
          .build()
          .parseClaimsJws(token)
          .getBody()
          .getSubject();
      log.debug("📝 토큰에서 userId 추출 성공: {}", userId);
      return userId;
    } catch (Exception e) {
      log.error("❌ 토큰에서 userId 추출 실패 - 오류: {}", e.getMessage(), e);
      throw e;
    }
  }

  // 토큰에서 role 추출
  public String getRoleFromToken(String token) {
    try {
      String role = (String) Jwts.parserBuilder()
          .setSigningKey(key)
          .build()
          .parseClaimsJws(token)
          .getBody()
          .get("role");
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
      Claims claims = Jwts.parserBuilder()
          .setSigningKey(key)
          .build()
          .parseClaimsJws(token)
          .getBody();
      
      String userId = claims.getSubject();
      String algorithm = "HS256"; // JWT 헤더에서 알고리즘 확인
      log.info("✅ JWT 토큰 검증 성공 - userId: {}, 알고리즘: {}", userId, algorithm);
      return true;
    } catch (SecurityException | MalformedJwtException e) {
      log.error("❌ JWT 서명 오류 또는 잘못된 토큰 형식 - 오류: {}, 토큰 앞 20자: {}", 
          e.getMessage(), token.length() > 20 ? token.substring(0, 20) + "..." : token);
      log.error("❌ 사용된 Secret Key 길이: {} bytes (원본), 알고리즘: HS256", 
          originalSecretKey != null ? originalSecretKey.getBytes().length : 0);
    } catch (ExpiredJwtException e) {
      log.warn("⏰ JWT 토큰 만료 - userId: {}, 만료시간: {}", 
          e.getClaims().getSubject(), e.getClaims().getExpiration());
    } catch (UnsupportedJwtException e) {
      log.error("❌ 지원하지 않는 JWT 형식 - 오류: {}", e.getMessage());
    } catch (IllegalArgumentException e) {
      log.error("❌ JWT 토큰이 비어있거나 잘못된 형식 - 오류: {}", e.getMessage());
    } catch (Exception e) {
      log.error("❌ JWT 검증 중 예상치 못한 오류 발생 - 오류 타입: {}, 메시지: {}", 
          e.getClass().getSimpleName(), e.getMessage(), e);
    }
    return false;
  }

  // 스프링 시큐리티 인증 객체에 필요한 권한(Authority) 생성
  public Collection<? extends GrantedAuthority> getAuthorities(String role) {
    return List.of(new SimpleGrantedAuthority("ROLE_" + role));
  }
}
