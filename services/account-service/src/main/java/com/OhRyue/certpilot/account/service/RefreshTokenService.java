package com.OhRyue.certpilot.account.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
public class RefreshTokenService {

    private final StringRedisTemplate redisTemplate;
    private final long REFRESH_TOKEN_EXPIRATION = 7L; // 7일

    // Refresh Token 저장
    public void save(String username, String refreshToken) {
        redisTemplate.opsForValue().set(
                "RT:" + username,
                refreshToken,
                REFRESH_TOKEN_EXPIRATION,
                TimeUnit.DAYS
        );
    }

    // Refresh Token 삭제(로그아웃 / 재발급 실패 등)
    public String get(String username) {
        return redisTemplate.opsForValue().get("RT:" + username);
    }

    // Redis에 저장된 Refresh Token 조회
    public void delete(String username) {
        redisTemplate.delete("RT:" + username);
    }
}
