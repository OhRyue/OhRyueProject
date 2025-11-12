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

    private final RedisTemplate<String, String> redisTemplate;

    public VerificationCodeService(RedisTemplate<String, String> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    // 인증코드 저장 (email 또는 username 기준)
    public void saveCode(String email, String code) {
        String key = "VC:" + email;  // Verification Code
        redisTemplate.opsForValue().set(key, code, expireMinutes, TimeUnit.MINUTES);
    }

    // 코드 가져오기
    public String getCode(String email) {
        return redisTemplate.opsForValue().get("VC:" + email);
    }

    // 인증 완료 후 삭제
    public void deleteCode(String email) {
        redisTemplate.delete("VC:" + email);
    }

    public String generateCode(String email) {
        String code = String.format("%06d", new Random().nextInt(999999));
        redisTemplate.opsForValue().set(email, code, Duration.ofMinutes(5));
        return code;
    }

}
