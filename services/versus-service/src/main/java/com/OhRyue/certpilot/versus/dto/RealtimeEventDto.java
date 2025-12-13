package com.OhRyue.certpilot.versus.dto;

import com.OhRyue.certpilot.versus.domain.MatchEvent;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.time.Instant;
import java.util.Map;

/**
 * WebSocket을 통한 실시간 이벤트 DTO
 * 
 * match_event 테이블의 내용을 그대로 WebSocket으로 브로드캐스트하기 위한 DTO
 * 프론트엔드가 구독할 수 있도록 간단한 구조로 설계
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public record RealtimeEventDto(
        /**
         * 이벤트 ID (match_event.id)
         */
        Long eventId,

        /**
         * 방 ID (roomId)
         */
        Long roomId,

        /**
         * 이벤트 타입
         * 예: PLAYER_JOINED, MATCH_STARTED, QUESTION_STARTED, ANSWER_SUBMITTED, 
         *     PLAYER_ELIMINATED, MATCH_FINISHED 등
         */
        String eventType,

        /**
         * 이벤트 페이로드 (JSON 파싱된 Map)
         * match_event.payload_json을 파싱한 결과
         */
        Map<String, Object> payload,

        /**
         * 이벤트 생성 시각 (ISO-8601 형식)
         */
        Instant createdAt
) {
    /**
     * MatchEvent 도메인을 RealtimeEventDto로 변환
     * 
     * @param event MatchEvent 도메인 객체
     * @param payload 파싱된 payload (null 가능)
     * @return RealtimeEventDto
     */
    public static RealtimeEventDto from(MatchEvent event, Map<String, Object> payload) {
        return new RealtimeEventDto(
                event.getId(),
                event.getRoomId(),
                event.getEventType(),
                payload,
                event.getCreatedAt()
        );
    }
}

