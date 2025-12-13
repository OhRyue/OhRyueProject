package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.dto.RealtimeEventDto;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 실시간 이벤트 브로드캐스트 서비스
 * 
 * 멀티 인스턴스 환경에서 이벤트를 모든 클라이언트에게 동일하게 전달하기 위한 서비스
 * 
 * 이벤트 발생 흐름:
 * 1. DB에 이벤트 저장
 * 2. Redis Pub/Sub으로 publish (versus:room:{roomId})
 * 3. 모든 인스턴스가 Redis에서 수신
 * 4. 각 인스턴스가 WebSocket으로 브로드캐스트 (/topic/versus/rooms/{roomId})
 * 
 * 게임 로직은 건드리지 않고, 이벤트 기록 후 브로드캐스트만 수행하는 읽기 전용 실시간 서비스입니다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RealtimeEventService {

    private static final String TOPIC_PREFIX = "/topic/versus/rooms";
    private static final String REDIS_CHANNEL_PREFIX = "versus:room:";

    private final SimpMessagingTemplate messagingTemplate;
    private final StringRedisTemplate redisTemplate;
    private final ObjectMapper objectMapper;

    /**
     * MatchEvent를 Redis Pub/Sub으로 발행
     * 
     * 멀티 인스턴스 환경에서 모든 인스턴스가 이벤트를 수신할 수 있도록 Redis로 publish
     * 
     * @param event 저장된 MatchEvent 객체 (ID가 있어야 함)
     */
    public void broadcastEvent(MatchEvent event) {
        if (event == null || event.getRoomId() == null) {
            log.warn("RealtimeEventService: Invalid event - event or roomId is null");
            return;
        }

        if (event.getId() == null) {
            log.warn("RealtimeEventService: Event ID is null, skipping broadcast. roomId={}, eventType={}",
                    event.getRoomId(), event.getEventType());
            return;
        }

        try {
            // Payload JSON 파싱
            Map<String, Object> payload = parsePayload(event.getPayloadJson());

            // RealtimeEventDto 생성
            RealtimeEventDto eventDto = RealtimeEventDto.from(event, payload);

            // RealtimeEventDto를 JSON 문자열로 변환
            String eventJson = objectMapper.writeValueAsString(eventDto);

            // Redis 채널: versus:room:{roomId}
            String channel = REDIS_CHANNEL_PREFIX + event.getRoomId();

            // Redis Pub/Sub으로 발행
            redisTemplate.convertAndSend(channel, eventJson);

            log.debug("RealtimeEventService: Event published to Redis - roomId={}, eventType={}, channel={}",
                    event.getRoomId(), event.getEventType(), channel);

        } catch (Exception e) {
            // Redis publish 실패는 로그만 남기고 예외를 전파하지 않음
            // (DB 저장은 성공했으므로 실시간 브로드캐스트 실패가 게임 로직에 영향을 주면 안 됨)
            log.warn("RealtimeEventService: Failed to publish event to Redis - roomId={}, eventType={}, error={}",
                    event.getRoomId(), event.getEventType(), e.getMessage(), e);
        }
    }

    /**
     * Payload JSON 문자열을 Map으로 파싱
     * 
     * @param payloadJson JSON 문자열 (null 가능)
     * @return 파싱된 Map (null이거나 빈 문자열이면 빈 Map 반환)
     */
    private Map<String, Object> parsePayload(String payloadJson) {
        if (payloadJson == null || payloadJson.isBlank()) {
            return Map.of();
        }

        try {
            return objectMapper.readValue(payloadJson, new TypeReference<Map<String, Object>>() {});
        } catch (Exception e) {
            log.warn("RealtimeEventService: Failed to parse payload JSON - error={}", e.getMessage());
            // 파싱 실패 시 원본 JSON을 "raw" 키에 저장
            return Map.of("raw", payloadJson);
        }
    }
}





