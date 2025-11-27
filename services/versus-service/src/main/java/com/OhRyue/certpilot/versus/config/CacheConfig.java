package com.OhRyue.certpilot.versus.config;

import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 캐싱 설정
 * - 문제 정보 캐싱 (study-service 호출 감소)
 * - 스코어보드 캐싱 (빈번한 조회 최적화)
 */
@Configuration
@EnableCaching
public class CacheConfig {

    @Bean
    public CacheManager cacheManager() {
        return new ConcurrentMapCacheManager(
            "versus-scoreboard",  // 스코어보드 캐시
            "question-info",      // 문제 정보 캐시
            "room-detail"         // 방 상세 정보 캐시
        );
    }
}


