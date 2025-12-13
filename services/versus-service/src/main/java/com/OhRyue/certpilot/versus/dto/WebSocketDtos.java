package com.OhRyue.certpilot.versus.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;

/**
 * WebSocket 메시지 DTO
 */
public class WebSocketDtos {

    /**
     * JOIN_ROOM 명령 메시지
     * 
     * 클라이언트 -> 서버: /app/versus/join
     * 
     * 예시:
     * {
     *   "command": "JOIN_ROOM",
     *   "roomId": 123
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record JoinRoomCommand(
            @NotNull
            Long roomId
    ) {
        public static final String COMMAND = "JOIN_ROOM";
    }

    /**
     * JOIN_ROOM 응답 메시지
     * 
     * 서버 -> 클라이언트: /user/queue/versus/join
     * 
     * 예시:
     * {
     *   "type": "JOIN_ROOM_RESPONSE",
     *   "success": true,
     *   "roomId": 123,
     *   "snapshot": { ... }
     * }
     */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record JoinRoomResponse(
            String type,
            boolean success,
            Long roomId,
            String message,
            RoomSnapshotDto snapshot
    ) {
        public static final String TYPE = "JOIN_ROOM_RESPONSE";

        public static JoinRoomResponse success(Long roomId, RoomSnapshotDto snapshot) {
            return new JoinRoomResponse(TYPE, true, roomId, null, snapshot);
        }

        public static JoinRoomResponse failure(Long roomId, String message) {
            return new JoinRoomResponse(TYPE, false, roomId, message, null);
        }
    }
}

