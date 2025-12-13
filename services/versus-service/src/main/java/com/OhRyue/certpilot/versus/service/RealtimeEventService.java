package com.OhRyue.certpilot.versus.service;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.OhRyue.certpilot.versus.dto.RealtimeEventDto;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 실시간 이벤트 브로드캐스트 서비스
 * 
 * match_event 테이블에 저장된 이벤트를 WebSocket을 통해 실시간으로 브로드캐스트합니다.
 * Topic: /topic/versus/rooms/{roomId}
 * 
 * 게임 로직은 건드리지 않고, 이벤트 기록 후 브로드캐스트만 수행하는 읽기 전용 실시간 서비스입니다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class RealtimeEventService {

    private static final String TOPIC_PREFIX = "/topic/versus/rooms";

    private final SimpMessagingTemplate messagingTemplate;
    private final ObjectMapper objectMapper;

    /**
     * MatchEvent를 WebSocket으로 브로드캐스트
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

            // Topic 경로 생성: /topic/versus/rooms/{roomId}
            String topic = String.format("%s/%d", TOPIC_PREFIX, event.getRoomId());

            // WebSocket으로 브로드캐스트
            messagingTemplate.convertAndSend(topic, eventDto);

            log.debug("RealtimeEventService: Event broadcasted - roomId={}, eventType={}, topic={}",
                    event.getRoomId(), event.getEventType(), topic);

        } catch (Exception e) {
            // WebSocket 브로드캐스트 실패는 로그만 남기고 예외를 전파하지 않음
            // (DB 저장은 성공했으므로 실시간 브로드캐스트 실패가 게임 로직에 영향을 주면 안 됨)
            log.warn("RealtimeEventService: Failed to broadcast event - roomId={}, eventType={}, error={}",
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




