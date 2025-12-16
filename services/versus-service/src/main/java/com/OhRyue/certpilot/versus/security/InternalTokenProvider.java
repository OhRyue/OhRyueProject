package com.OhRyue.certpilot.versus.security;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.util.Date;
import java.util.List;

/**
 * 서비스 간 내부 호출용 JWT 토큰 생성기
 *
 * Internal JWT Claims:
 * - sub: internal-versus-service
 * - roles: ["INTERNAL"]
 * - iss: certpilot-internal
 * - aud: study-service | progress-service (대상별로 다르게)
 * - exp: 10~30분 (설정값)
 *
 * 알고리즘: HS256
 */
@Component
@Slf4j
public class InternalTokenProvider {

    private static final String SUBJECT = "internal-versus-service";
    private static final String ROLE_INTERNAL = "INTERNAL";
    private static final int DEFAULT_EXPIRATION_MINUTES = 15;

    private final SecretKey secretKey;
    private final String issuer;
    private final int expirationMinutes;

    public InternalTokenProvider(
            @Value("${auth.internal.jwt.secret:}") String secret,
            @Value("${auth.internal.jwt.issuer:certpilot-internal}") String issuer,
            @Value("${auth.internal.jwt.expiration-minutes:15}") int expirationMinutes
    ) {
        if (secret == null || secret.isBlank()) {
            throw new IllegalArgumentException("auth.internal.jwt.secret must be set");
        }

        // base64 우선 디코드 (현재 .env가 base64)
        byte[] keyBytes = decodeSecret(secret);
        this.secretKey = Keys.hmacShaKeyFor(keyBytes);

        this.issuer = issuer;
        this.expirationMinutes = expirationMinutes > 0 ? expirationMinutes : DEFAULT_EXPIRATION_MINUTES;

        log.info("InternalTokenProvider initialized: issuer={}, expiration={} minutes",
                this.issuer, this.expirationMinutes);
    }

    /**
     * 대상 서비스 audience를 받아서 internal 토큰 생성
     * @param audience "study-service" | "progress-service"
     */
    public String generateInternalToken(String audience) {
        if (audience == null || audience.isBlank()) {
            throw new IllegalArgumentException("audience must not be blank");
        }

        long now = System.currentTimeMillis();
        Date issuedAt = new Date(now);
        Date expiration = new Date(now + expirationMinutes * 60L * 1000);

        String token = Jwts.builder()
                .setSubject(SUBJECT)
                .setIssuer(issuer)                      // 표준 클레임
                .setAudience(audience)                  // 표준 클레임
                .claim("roles", List.of(ROLE_INTERNAL)) // List로 고정
                .setIssuedAt(issuedAt)
                .setExpiration(expiration)
                .signWith(secretKey, SignatureAlgorithm.HS256)
                .compact();

        log.debug("Internal JWT generated: sub={}, iss={}, aud={}, expMin={}",
                SUBJECT, issuer, audience, expirationMinutes);

        return token;
    }

    /** (옵션) 기존 호출부 호환용: 기본 audience=study-service */
    public String generateInternalToken() {
        return generateInternalToken("study-service");
    }

    private byte[] decodeSecret(String secret) {
        try {
            return Decoders.BASE64.decode(secret);
        } catch (Exception e) {
            return secret.getBytes();
        }
    }
}
