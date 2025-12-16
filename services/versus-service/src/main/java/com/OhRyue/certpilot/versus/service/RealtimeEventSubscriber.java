package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.dto.RealtimeEventDto;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;

/**
 * Redis Pub/Sub 이벤트 구독자
 *
 * Redis에서 수신한 이벤트를 WebSocket으로 브로드캐스트
 *
 * 채널: versus:room:{roomId}
 * - 모든 인스턴스가 구독
 * - 수신한 이벤트를 WebSocket으로 브로드캐스트
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RealtimeEventSubscriber implements MessageListener {

    private static final String TOPIC_PREFIX = "/topic/versus/rooms";

    private final SimpMessagingTemplate messagingTemplate;
    private final ObjectMapper objectMapper;

    /**
     * Redis에서 수신한 메시지 처리
     *
     * RedisMessageListenerContainer가 호출하는 메서드
     * - 시그니처: (Message message, byte[] pattern)
     * - Spring Data Redis 표준 인터페이스
     *
     * @param message Redis에서 수신한 메시지
     * @param pattern 구독한 패턴
     */
    @Override
    public void onMessage(Message message, byte[] pattern) {
        try {
            if (message == null || message.getBody() == null) {
                log.warn("RealtimeEventSubscriber: Received null Redis message");
                return;
            }

            String messageStr = new String(message.getBody(), StandardCharsets.UTF_8);
            String channelStr = message.getChannel() != null
                    ? new String(message.getChannel(), StandardCharsets.UTF_8)
                    : "unknown";
            String patternStr = pattern != null
                    ? new String(pattern, StandardCharsets.UTF_8)
                    : "unknown";

            log.debug(
                "RealtimeEventSubscriber: Message received - channel={}, pattern={}, messageLength={}",
                channelStr,
                patternStr,
                messageStr.length()
            );

            // JSON 문자열을 RealtimeEventDto로 파싱
            RealtimeEventDto eventDto = objectMapper.readValue(
                    messageStr,
                    RealtimeEventDto.class
            );

            if (eventDto.roomId() == null) {
                log.warn("RealtimeEventSubscriber: Invalid event - roomId is null");
                return;
            }

            // Topic 경로 생성: /topic/versus/rooms/{roomId}
            String topic = String.format("%s/%d", TOPIC_PREFIX, eventDto.roomId());

            // WebSocket으로 브로드캐스트
            messagingTemplate.convertAndSend(topic, eventDto);

            log.debug(
                "RealtimeEventSubscriber: Event broadcasted - roomId={}, eventType={}, topic={}",
                eventDto.roomId(),
                eventDto.eventType(),
                topic
            );

        } catch (Exception e) {
            log.error(
                "RealtimeEventSubscriber: Failed to process Redis message - error={}",
                e.getMessage(),
                e
            );
        }
    }
}
