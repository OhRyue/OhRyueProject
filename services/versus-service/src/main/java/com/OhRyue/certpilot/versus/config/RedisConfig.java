package com.OhRyue.certpilot.versus.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.StringRedisSerializer;

/**
 * Redis 설정 클래스
 * 
 * - lettuce 클라이언트 사용 (비동기, 논블로킹)
 * - spring-data-redis 기반
 * - WebSocket 메시징 및 세션 관리에 사용
 * 
 * docker-compose 환경:
 *   - host: redis (서비스 이름)
 *   - port: 6379
 * 
 * 로컬 환경:
 *   - host: localhost
 *   - port: 6379
 */
@Configuration
public class RedisConfig {

    @Value("${spring.data.redis.host:localhost}")
    private String redisHost;

    @Value("${spring.data.redis.port:6379}")
    private int redisPort;

    /**
     * Lettuce 기반 Redis 연결 팩토리
     * - Lettuce는 비동기, 논블로킹 방식으로 성능이 우수
     * - 멀티스레드 환경에서 안전하게 사용 가능
     */
    @Bean
    public RedisConnectionFactory redisConnectionFactory() {
        RedisStandaloneConfiguration config = new RedisStandaloneConfiguration();
        config.setHostName(redisHost);
        config.setPort(redisPort);
        return new LettuceConnectionFactory(config);
    }

    /**
     * RedisTemplate 설정
     * - Key/Value 모두 String 타입으로 직렬화
     * - WebSocket 메시징 및 세션 관리에 사용
     */
    @Bean
    public RedisTemplate<String, String> redisTemplate(RedisConnectionFactory connectionFactory) {
        RedisTemplate<String, String> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory);
        
        // Key/Value 모두 String 직렬화
        template.setKeySerializer(new StringRedisSerializer());
        template.setValueSerializer(new StringRedisSerializer());
        template.setHashKeySerializer(new StringRedisSerializer());
        template.setHashValueSerializer(new StringRedisSerializer());
        
        template.afterPropertiesSet();
        return template;
    }
}


