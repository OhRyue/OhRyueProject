package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.dto.RoomSnapshotDto;
import com.OhRyue.certpilot.versus.dto.WebSocketDtos;
import com.OhRyue.certpilot.versus.service.RoomSnapshotService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.annotation.SendToUser;
import org.springframework.stereotype.Controller;

import java.security.Principal;

/**
 * WebSocket 메시지 핸들러
 * 
 * STOMP 기반 WebSocket 메시징 처리
 * 
 * 엔드포인트:
 * - 클라이언트 -> 서버: /app/versus/join
 * - 서버 -> 클라이언트: /user/queue/versus/join
 */
@Controller
@RequiredArgsConstructor
@Slf4j
public class WebSocketController {

    private final RoomSnapshotService roomSnapshotService;

    /**
     * JOIN_ROOM 명령 처리
     * 
     * 클라이언트가 방에 입장할 때 호출
     * - roomId를 받아서 현재 방 상태 스냅샷을 반환
     * - userId는 WebSocket 인증 정보(Principal)에서 추출
     * 
     * @param command JOIN_ROOM 명령 (roomId 포함)
     * @param principal WebSocket 인증 정보 (JWT에서 추출된 userId)
     * @return JOIN_ROOM 응답 (스냅샷 포함)
     */
    @MessageMapping("/versus/join")
    @SendToUser("/queue/versus/join")
    public WebSocketDtos.JoinRoomResponse handleJoinRoom(
            @Payload WebSocketDtos.JoinRoomCommand command,
            Principal principal
    ) {
        String userId = principal != null ? principal.getName() : null;
        Long roomId = command.roomId();

        log.info("WebSocket JOIN_ROOM 요청: userId={}, roomId={}", userId, roomId);

        if (roomId == null) {
            log.warn("JOIN_ROOM: roomId가 null입니다. userId={}", userId);
            return WebSocketDtos.JoinRoomResponse.failure(null, "roomId는 필수입니다.");
        }

        try {
            // 방 상태 스냅샷 생성
            RoomSnapshotDto snapshot = roomSnapshotService.createSnapshot(roomId);

            log.info("JOIN_ROOM 성공: userId={}, roomId={}, status={}",
                    userId, roomId, snapshot.room().status());

            return WebSocketDtos.JoinRoomResponse.success(roomId, snapshot);

        } catch (IllegalArgumentException e) {
            log.warn("JOIN_ROOM 실패: userId={}, roomId={}, error={}",
                    userId, roomId, e.getMessage());
            return WebSocketDtos.JoinRoomResponse.failure(roomId, e.getMessage());

        } catch (Exception e) {
            log.error("JOIN_ROOM 오류: userId={}, roomId={}", userId, roomId, e);
            return WebSocketDtos.JoinRoomResponse.failure(roomId, "방 입장 중 오류가 발생했습니다.");
        }
    }
}

