package com.OhRyue.certpilot.versus.config;

import com.OhRyue.certpilot.versus.service.RealtimeEventSubscriber;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;

/**
 * Redis Pub/Sub 설정
 * 
 * 멀티 인스턴스 환경에서 이벤트를 모든 클라이언트에게 동일하게 전달하기 위한 설정
 * 
 * 채널: versus:room:{roomId}
 * - 각 방마다 별도의 채널 사용
 * - 패턴 구독: versus:room:*
 */
@Configuration
@RequiredArgsConstructor
public class RedisPubSubConfig {

    private static final String CHANNEL_PATTERN = "versus:room:*";

    private final RedisConnectionFactory redisConnectionFactory;
    private final RealtimeEventSubscriber realtimeEventSubscriber;

    /**
     * Redis 메시지 리스너 컨테이너
     * 
     * 모든 인스턴스가 Redis 채널을 구독하여 이벤트를 수신
     */
    @Bean
    public RedisMessageListenerContainer redisMessageListenerContainer() {
        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(redisConnectionFactory);

        // 패턴 구독: versus:room:*
        // 모든 방의 이벤트를 수신
        MessageListenerAdapter adapter = new MessageListenerAdapter(
                realtimeEventSubscriber,
                "onMessage"
        );
        
        container.addMessageListener(adapter, new PatternTopic(CHANNEL_PATTERN));

        return container;
    }
}

