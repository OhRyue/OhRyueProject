package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.dto.RealtimeEventDto;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;

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
public class RealtimeEventSubscriber {

    private static final String TOPIC_PREFIX = "/topic/versus/rooms";

    private final SimpMessagingTemplate messagingTemplate;
    private final ObjectMapper objectMapper;

    /**
     * Redis에서 수신한 메시지 처리
     * 
     * @param message Redis에서 수신한 메시지 (JSON 문자열)
     * @param pattern 구독한 패턴 (versus:room:*)
     */
    public void onMessage(String message, String pattern) {
        try {
            // JSON 문자열을 RealtimeEventDto로 파싱
            RealtimeEventDto eventDto = objectMapper.readValue(
                    message, 
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

            log.debug("RealtimeEventSubscriber: Event received from Redis and broadcasted - roomId={}, eventType={}, topic={}",
                    eventDto.roomId(), eventDto.eventType(), topic);

        } catch (Exception e) {
            log.error("RealtimeEventSubscriber: Failed to process message from Redis - pattern={}, error={}",
                    pattern, e.getMessage(), e);
        }
    }
}



