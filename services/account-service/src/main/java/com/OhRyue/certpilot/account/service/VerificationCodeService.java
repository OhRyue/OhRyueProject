package com.OhRyue.certpilot.account.service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.Random;
import java.util.concurrent.TimeUnit;

@Service
public class VerificationCodeService {

    @Value("${verification.expire-minutes:10}")
    private long expireMinutes;

    private static final String SIGNUP_PREFIX = "VC:SIGNUP:";
    private static final String RESET_PREFIX  = "VC:RESET:";

    private final RedisTemplate<String, String> redisTemplate;

    public VerificationCodeService(RedisTemplate<String, String> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    /* ========== 회원가입용 인증 코드 ========== */

    // 회원가입용 코드 저장 (email 기준)
    public void saveCode(String email, String code) {
        String key = SIGNUP_PREFIX + normalizeEmail(email);
        redisTemplate.opsForValue().set(key, code, expireMinutes, TimeUnit.MINUTES);
    }

    // 회원가입용 코드 조회
    public String getCode(String email) {
        String key = SIGNUP_PREFIX + normalizeEmail(email);
        return redisTemplate.opsForValue().get(key);
    }

    // 회원가입용 코드 삭제
    public void deleteCode(String email) {
        String key = SIGNUP_PREFIX + normalizeEmail(email);
        redisTemplate.delete(key);
    }

    /* ========== 비밀번호 찾기용 인증 코드 ========== */

    // 비밀번호 찾기용 코드 생성 + 저장
    public String generateResetCode(String email) {
        String normalized = normalizeEmail(email);
        String code = String.format("%06d", new Random().nextInt(999999));
        String key = RESET_PREFIX + normalized;
        redisTemplate.opsForValue()
                .set(key, code, Duration.ofMinutes(expireMinutes));
        return code;
    }

    // 비밀번호 찾기용 코드 검증
    public boolean verifyResetCode(String email, String code) {
        String normalized = normalizeEmail(email);
        String key = RESET_PREFIX + normalized;
        String stored = redisTemplate.opsForValue().get(key);
        if (stored != null && stored.equals(code)) {
            redisTemplate.delete(key); // 일회용
            return true;
        }
        return false;
    }

    private String normalizeEmail(String email) {
        return email == null ? null : email.trim().toLowerCase();
    }
}
